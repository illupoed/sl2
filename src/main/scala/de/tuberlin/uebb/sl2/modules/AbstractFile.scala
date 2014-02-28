package de.tuberlin.uebb.sl2.modules

import java.io.File
import java.net.URL
import scalax.file.FileSystem
import scalax.file.Path
import scala.io.Source

trait AbstractFile {
  /**
   * Used to abstract from the differences between files from a .jar file ({@link BottledFile})
   * and files from the file system ({@link PathedFile}).
   */
  abstract class AbstractFile {

    /**
     * undocumented
     */
    def filename(): String

    /**
     * undocumented
     */
    def parent(): File

    /**
     * undocumented
     */
    def path(): String

    /**
     * undocumented
     */
    def canRead(): Boolean

    /**
     * undocumented
     */
    def lastModified(): Long

    /**
     * undocumented
     */
    def contents(): String
  }

  class PathedFile( path: Path )
      extends AbstractFile {

    /**
     * undocumented
     */
    def filename() = {
      path.name
    }

    /**
     * undocumented
     */
    def parent() = {
      if ( path.parent != null )
        new File( path.parent.toString )
      else
        new File( "." )
    }

    /**
     * undocumented
     */
    def path() = {
      path.path
    }

    /**
     * undocumented
     */
    def canRead() = {
      path.canRead
    }

    /**
     * undocumented
     */
    def lastModified() = {
      path.lastModified
    }

    /**
     * undocumented
     */
    def contents() = {
      path.lines( includeTerminator = true ).mkString( "" )
    }
  }

  class ErrorFile( url: URL )
      extends AbstractFile {

    /**
     * undocumented
     */
    def filename = { "Error" }

    /**
     * undocumented
     */
    def parent = { new File( "." ) }

    /**
     * undocumented
     */
    def path = { throw new UnsupportedOperationException() }

    /**
     * undocumented
     */
    def canRead() = { false }

    /**
     * undocumented
     */
    def lastModified() = { 0 }

    /**
     * undocumented
     */
    def contents() = { throw new UnsupportedOperationException() }
  }

  /**
   * A file inside a .jar file
   */
  class BottledFile( url: URL )
      extends AbstractFile {

    /**
     * undocumented
     */
    def filename() = {
      val f = url.getFile
      f.substring( f.lastIndexOf( "!" ) + 1 )
    }

    /**
     * undocumented
     */
    def parent() = {
      new File( "." )
    }

    /**
     * undocumented
     */
    def path() = {
      url.toString
    }

    /**
     * undocumented
     */
    def jarFile() = {
      val f = url.getFile
      f.substring( 5, f.lastIndexOf( "!" ) )
    }

    /**
     * A bottled file can always be read. Otherwise, its location wouldn't have been found.
     */
    def canRead() = {
      true
    }

    /**
     * The modification date for a bottled does not matter, because all files
     * (source, js and signature) in a JAR file have the same date and cannot
     * be modified.
     */
    def lastModified() = {
      0
    }

    /**
     * undocumented
     */
    def contents() = {
      Source.fromURL( url ).getLines.mkString( "\n" )
    }
  }

  /**
   * undocumented
   */
  def createFile( url: URL, path: String ): AbstractFile = {
    createFile( new URL( url, path ) )
  }

  /**
   * undocumented
   */
  def createFile( file: File, path: String ): AbstractFile = {
    if ( file != null )
      createFile( new URL( file.toURI.toURL, path ) )
    else
      new ErrorFile( new File( path ).toURI.toURL )
  }

  /**
   * undocumented
   */
  def createFile( url: URL ): AbstractFile = {
    if ( url.toString.startsWith( "jar:" ) ) {
      new BottledFile( url )
    }
    else {
      val option = Path( url.toURI )
      if ( option.isEmpty )
        new ErrorFile( url )
      else
        new PathedFile( option.get )
    }
  }
}