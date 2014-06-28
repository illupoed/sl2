/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.impl

import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.modules.Syntax.{ VarFirstClass }
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.MacroConfig
import de.tuberlin.uebb.sl2.slmacro.sl_function.{ isAllowedFunctionName, isAllowedClassPath }
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.annotation.tailrec
import scala.reflect.macros.Universe
import scala.reflect.macros.{ Context => MacroCtxt }
import java.io.File
import java.security.MessageDigest
import scalax.io._
import java.util.regex.Pattern
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.util.Random
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * Driver for the `slci' macro.
 */
object MacroDriver
    extends CombinatorParser
    with MultiDriver
    with Configs
    with AbstractFile
    with CodeGenerator
    with Syntax
    with ProgramChecker
    with JsSyntax
    with Errors
    with SignatureSerializer
    with DebugOutput
    with ModuleResolver
    with ModuleNormalizer
    with ModuleLinearization
    with PreProcessing
    with NameSupply
    with Graph[VarFirstClass]
    with Context
    with Type
    with SyntaxTraversal
    with EnrichedLambdaCalculus
    with Substitution
    with Unification
    with MacroConfig

    // Implementations 
    with GraphImpl[VarFirstClass]
    with DTCheckerImpl
    with FDCheckerImpl
    with TypeCheckerImpl
    with ProgramCheckerImpl
    with LetRecSplitter

    with ModuleResolverImpl
    with ModuleNormalizerImpl
    with ModuleContextImpl
    with SignatureJsonSerializer {

  /**
   * main to test stuff
   */
  def main( input: Array[String] ): Unit =
    {

    }

  /**
   * the implementation of the macro
   */
  def macroImpl( c: MacroCtxt )( sl_code: c.Expr[String], vals: c.Expr[Any]* ): c.Expr[String] =
    {
      import c.universe._
      import de.tuberlin.uebb.sl2.slmacro.variabletranslation._

        // ----------------------------------------------------------------------
        // ----- Helper functions -----------------------------------------------
        // the're defined here to use the macro context

        /**
         * a helper to test a variable (macro parameter) against a list of translators
         * the function returns the first 'match' in the list
         */
        def useTranslators( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, c.Expr[Any => JValue] )] =
          {
            AbstractTranslator.useTranslators( c )( input, translators ) match {
              case Some( ( sl_type, _, translation_fun_scala_to_sl, _ ) ) => Some( ( sl_type, translation_fun_scala_to_sl ) )
              case None => None
            }
          }

        /**
         * TODO add comments
         */
        def renderFunctionImports( sl_imports: Seq[QualifiedImport] ): List[c.Tree] = {
          val imports = for ( sl_import <- sl_imports if ( sl_import.path.matches( "^" + annotation_sl_macro_folder + ".*" ) ) ) yield {
            getClassAndFunctionFromGeneratedSLFile( sl_import.path ) match {
              case Left( err_msg ) => c.abort( c.enclosingPosition, err_msg )
              case Right( ( class_path, function_name ) ) => {
                //println( class_path )
                renderFunctionImport( class_path, function_name )
              }
            }
          }

          imports.toList
        }

        /**
         * creates AST to import a function under a pseudo random alias
         */
        def renderFunctionImport( class_path: String, function_name: String ): c.Tree =
          {
              def helper( class_path: Seq[String] ): c.Tree =
                {
                  //println( class_path )

                  if ( class_path.length == 1 )
                    Ident( newTermName( class_path.head ) )
                  else
                    Select(
                      helper( class_path.tail ),
                      newTermName( class_path.head )
                    )
                }
            val rand = new Random()
            val function_alias: String = "fun" + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 ) + rand.nextInt( 9 )
            val class_path_splitted = class_path.split( "\\." ).reverse.toSeq
            //for ( i <- class_path_splitted ) println( i )

            c.universe.Import(
              helper( class_path_splitted ),
              List(
                ImportSelector(
                  newTermName( function_name ),
                  -1,
                  newTermName( function_alias ),
                  -1
                )
              )
            )
          }

        /**
         * creates a valid ast for a String
         */
        def getExpressionFromConstantString( input: String ): c.Expr[String] =
          {
            c.Expr[String]( Literal( Constant( input ) ) )
          }

        /**
         * creates a valid ast for applying an anonymous function : Any => String to an arg : Any
         */
        def getExpressionApply( f: c.Expr[Any => JValue], arg: c.Expr[Any] ): c.Expr[String] =
          {
            c.Expr[String]( q"org.json4s.native.JsonMethods.compact(org.json4s.native.JsonMethods.render($f($arg)))" )
          }

        /**
         * Returns all QualifiedImports of a given code snippet
         */
        def getQualifiedImports( source_code: String ): Seq[QualifiedImport] =
          {
            val ast = parseAst( source_code )

            ast match {
              case Right( Program( imports, _, _, _, _, _ ) ) =>
                for ( imp <- imports if ( imp.isInstanceOf[QualifiedImport] ) )
                  yield imp.asInstanceOf[QualifiedImport]
              case Left( err ) => c.abort( c.enclosingPosition, errorMessage( err ) )
              case _ => c.abort( c.enclosingPosition, "Unknown error while compiling simple language source code." ) // this case should never happen 

            }
          }

        /**
         * recursive function to check/translate the given parameter of the macro
         *
         * @param parameter_number the number of the current parameter
         * @param number_of_placeholders number of placeholders in the sl code (how many parameter of the macro do we have to check)
         * @param vals the parameter of the macro as Expressions. vals.length >= number_of_placeholder !!!
         * @param translators a Seq of Translator implementations
         *
         * @return Either an error String or a Seq[(sl_name, sl_type, js_name, string_expr)] the string expr represents the value of a scala parameter in js
         */
        def handleScalaValues( parameter_number: Int, number_of_placeholders: Int, vals: Seq[c.Expr[Any]], translators: Seq[AbstractTranslator] ): Either[String, Seq[Tuple4[String, String, String, c.Expr[String]]]] =
          {
            // exit condition
            if ( parameter_number == number_of_placeholders )
              return Right( List() )

            val scala_val_name = vals.head.tree.toString()
            val scala_val_type = vals.head.actualType
            val translator_info = useTranslators( scala_val_type, translators )

            // recursion
            handleScalaValues( parameter_number + 1, number_of_placeholders, vals.tail, translators ) match {
              // recursion returns an error
              case Left( error_string ) =>
                {
                  if ( translator_info.isDefined )
                    // return the error from recursion
                    Left( error_string )
                  else
                    // prepend current error
                    Left( f"Parameter $parameter_number%s: Couldn't find an appropriate SL type for $scala_val_name%s : $scala_val_type%s!\n" + error_string )
                }
              case Right( list ) =>
                {
                  translator_info match {
                    case None =>
                      {
                        // return an error
                        Left( f"Parameter $parameter_number%s: Couldn't find an appropriate SL type for $scala_val_name%s : $scala_val_type%s!\n" )
                      }
                    case Some( ( sl_val_type, translation_expression ) ) =>
                      {
                        // everything is fine
                        val sl_val_name = "scalaParam" + parameter_number
                        val js_val_name = md5( c.enclosingPosition.source.toString() + c.enclosingPosition.line.toString() ) + sl_val_name
                        Right(
                          Tuple4(
                            sl_val_name,
                            sl_val_type,
                            js_val_name,
                            getExpressionApply( translation_expression, vals.head )
                          ) +: list
                        )
                      }
                  }
                }
            }
          }

      val preludeTranslators: Seq[AbstractTranslator] = AbstractTranslator.preludeTranslators

      sl_code match {
        case Expr( Literal( Constant( sl ) ) ) =>
          {
            val module_name = c.enclosingPosition.source.file.name + "." + c.enclosingPosition.line
            val filename = inline_sl_macro_folder + module_name + ".sl"

            // hack to remove all placeholders in comments
            var sl_code = sl.toString().replaceAll( "(?m)^--.*$", "--" );

            // check number of placeholders and given parameters
            val number_of_placeholders = countSubstring( sl_code, scala_var_placeholder )
            val test = number_of_placeholders - vals.length

            if ( test > 0 )
              c.abort( c.enclosingPosition, "There are more placeholders in the sl code then parameter!" )
            else {
              if ( test < 0 )
                c.warning( c.enclosingPosition, "There are more paremter given then placeholders in the sl code!" );
            }

            val sl_imports = getQualifiedImports( sl_code.replaceAll( Pattern.quote( scala_var_placeholder ), "{| |}" ) )

            // trying to transform all macro parameters to sl
            val handleResult = handleScalaValues(
              0,
              number_of_placeholders,
              vals,
              preludeTranslators ++ loadTranslatorsFromDependencies( sl_imports )
            )

            if ( handleResult.isLeft ) {
              // error handling
              c.abort( c.enclosingPosition, handleResult.left.get )
            }
            else {

              // adding sl value definitions for the macro parameters
              sl_code = sl_code + "\n\n-- Generated Methods for scala values --\n"
              val paramList = handleResult.right.get

              for ( param <- paramList ) {
                sl_code = ( sl_code.replaceFirst( Pattern.quote( scala_var_placeholder ), param._1 ) ) + renderSLDef( param._1, param._2, param._3 )
              }

              // write sl to tmp file
              writeSlToFile( assets_dir + filename, sl_code )

              //build config
              val config: Config = Config( new File( assets_dir ) // source path
              , filename :: List() // sources
              , new File( assets_dir ) // class path
              , "" // mainName
              , new File( "" ) // mainParent
              , new File( assets_dir ) // destination
              , false // generate a html file?
              )

              // compile sl code
              val res = run( config )

              if ( res.isLeft ) {
                // handle compilation error
                c.abort( c.enclosingPosition, errorMessage( res.left.get ) )
              }
              else {
                // the compiled sl code with replaced place holders
                val require = getExpressionFromConstantString( renderRequire( module_name ) + paramList.foldLeft( "\n// transformed scala variables \n" )( ( str, param_info ) => str + renderJSDef( param_info._3 ) ) )

                // translation functions ( as ast ) to translate a scala values to their appropriate sl-value (in js form)
                val expr_list = paramList.map( ( x ) => x._4.tree ).toList

                // we import the used scala functions (via sl annotation macro) into the block (to make sure that if the signature of that function changes we recompile this block/file)
                val imports = renderFunctionImports( sl_imports )

                println( "----- Compiled " + module_name + " -----------------" )
                c.Expr[String]( q"{import de.tuberlin.uebb.sl2.impl._; ..$imports ; $require.format (..$expr_list) }" )
              }
            }
          }
        case _ =>
          {
            c.abort( c.enclosingPosition, "Expected a string literal, got: " + showRaw( sl_code ) )
          }
      }
    }

  /**
   * helper returning sl code for a constant function
   */
  private def renderSLDef( sl_val_name: String, sl_val_type: String, js_val_name: String ): String =
    { f"\n\nFUN $sl_val_name%s : $sl_val_type%s\nDEF EXTERN $sl_val_name%s = {| sl['$js_val_name%s'] |}" }

  /**
   * helper returning js of a variable definition
   */
  private def renderJSDef( js_val_name: String ): String =
    { f"\nsl['$js_val_name%s'] = %%s;" }

  /**
   * calculates the md5 hash of a string
   */
  private def md5( input: String ): String = {
    val ar = MessageDigest.getInstance( "MD5" ).digest( input.getBytes() )

    ar.foldLeft( "" )( ( x, y ) => x + String.format( "%x", new Integer( y & 0xff ) ) )
  }

  /**
   * writes a String to a certain file. Deletes the file if it already exists.
   *
   * @param filepath eg. /tmp/foo.sl
   * @param content the content of the file
   */
  private def writeSlToFile( filepath: String, content: String ): Unit = {
    import scalax.file.Path

    val file = Path.fromString( filepath )

    file.createFile( createParents = true, failIfExists = false )

    file.write( content )
  }

  /**
   * with this macro you can integrate sl_code in a html document. It will generate a
   * javascript snippet which loads the compiled sl_code.
   *
   * @param sl_code the sl_code which should be integrated in this html document
   * @param vals* you can pass variables which will replace $s placeholders in the sl code (if it is possible. The macro has to translate them to sl values.)
   * @return String js code which will include the compiled sl code
   */
  def slci( sl_code: String, vals: Any* ): String = macro macroImpl

  /**
   * generates the js to include the sl code
   */
  private def renderRequire( module_name: String ): String =
    {
      f"""
require
  ( [ "$inline_sl_macro_folder%s$module_name%s.sl" ]
  , function (tmp)
    {
      tmp.$$main();
    }
  );
"""
    }

  /**
   * counts all occurrences of a substring in a string
   *
   * @see http://rosettacode.org/wiki/Count_occurrences_of_a_substring#Scala
   */
  private def countSubstring( haystack: String, needle: String ): Int = {
      @tailrec def count( pos: Int, c: Int ): Int = {
        val idx = haystack indexOf ( needle, pos )
        if ( idx == -1 )
          c
        else
          count( idx + needle.size, c + 1 )
      }
    count( 0, 0 )
  }

  /**
   * Analysis the sl code for import statements and loads the appropriate translator classes
   *
   * @param source_code sl code
   */
  private def loadTranslatorsFromDependencies( sl_imports: Seq[QualifiedImport] ): Seq[AbstractTranslator] =
    {
      import de.tuberlin.uebb.sl2.slmacro.variabletranslation

      for (
        imp <- sl_imports;
        trans <- AbstractModulTranslator.resolveImport( imp.path, imp.name )
      ) yield trans
    }

  /**
   * In every sl file generated by the sl annotation macro is defined for which object (package_path.class_path) and function_name it was generated.
   * This function returns these values
   * counterpart of this function is sl_function.defineSlFile()
   */
  private def getClassAndFunctionFromGeneratedSLFile( import_path: String ): Either[String, Pair[String, String]] =
    {
      val absolut_path = assets_dir + import_path + ".sl"
      try {
        val in = new FileInputStream( absolut_path );
        val br = new BufferedReader( new InputStreamReader( in ) );

        var class_path = ""
        var function_name = ""
        var i = 0;

        do {
          i = i + 1;
          val line = br.readLine();

          if ( line == null )
            return Left( f"Could not read file ($absolut_path%s) generated by the sl annotation macro" )

          i match {
            case 2 => class_path = line.substring( 7 ).trim
            case 3 => function_name = line.substring( 7 ).trim
            case _ =>
          }
        } while ( i < 3 )

        if ( !isAllowedFunctionName( function_name ) || !isAllowedClassPath( class_path ) )
          return Left( f"Could not find class_path and function_name in file ($absolut_path%s) generated by the annotation macro. Did you changed the contents of the file?" )
        else
          return Right( class_path, function_name )

      }
      catch {
        case e: Throwable =>
          {
            return Left( f"Could not read file ($absolut_path%s) generated by the sl annotation macro" )
          }
      }
    }

}
