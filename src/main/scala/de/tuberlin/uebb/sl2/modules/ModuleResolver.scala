package de.tuberlin.uebb.sl2.modules

import java.io.File
import java.net.URL

/**
 * A ModuleResolver is able to find and load the modules a program
 * references.
 */
trait ModuleResolver {
  this: Syntax with AbstractFile with Errors with Configs =>

  sealed abstract class ResolvedImport(
    val path: String,
    val file: AbstractFile,
    val ast: Import )

  sealed abstract class ResolvedModuleImport(
    val name: String,
    override val path: String,
    override val file: AbstractFile,
    val jsFile: AbstractFile,
    val signature: Program,
    override val ast: Import )
      extends ResolvedImport( path, file, ast )

  case class ResolvedUnqualifiedImport(
    override val path: String,
    override val file: AbstractFile,
    override val jsFile: AbstractFile,
    override val signature: Program,
    override val ast: UnqualifiedImport )
      extends ResolvedModuleImport(
        "$$" + path.replace( '/', '$' ), path, file, jsFile, signature, ast )

  case class ResolvedQualifiedImport(
    override val name: ModuleVar,
    override val path: String,
    override val file: AbstractFile,
    override val jsFile: AbstractFile,
    override val signature: Program,
    override val ast: QualifiedImport )
      extends ResolvedModuleImport( name, path, file, jsFile, signature, ast )

  case class ResolvedExternImport(
    override val path: String,
    override val file: AbstractFile,
    override val ast: ExternImport )
      extends ResolvedImport( path, file, ast )

  /**
   * undocumented
   */
  def inferDependencies( program: AST, config: Config ): Either[Error, List[ResolvedImport]]

  /**
   * undocumented
   */
  def resolveDependencies( program: AST, config: Config ): Either[Error, Set[String]]

  /**
   * undocumented
   */
  def checkImports( imports: List[Import] ): Either[Error, Unit]

  /**
   * undocumented
   */
  def findImportResource( path: String, config: Config, attr: Attribute ): Either[Error, AbstractFile]

  /**
   * undocumented
   */
  def standardLibName: String

  /**
   * undocumented
   */
  def standardLibUrl: String

  /**
   * undocumented
   */
  def getLibResource( path: String ) = getClass().getResource( standardLibUrl + path )
}
