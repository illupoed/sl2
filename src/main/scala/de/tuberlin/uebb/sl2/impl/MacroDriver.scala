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

import scala.collection.mutable.ListBuffer
import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.modules.Syntax.{ VarFirstClass }
import language.experimental.macros
import reflect.macros.{ Context => MacroCtxt }
import scala.io.Source
import java.io.File
import java.security.MessageDigest
import scalax.io._
import scala.annotation.tailrec
import java.util.regex.Pattern
import scala.reflect.macros.Universe
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator

/**
 * Driver for the `slc' macro.
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
      import scala.reflect.runtime.universe._
      println( showRaw( reify( "foo" ) ) )

    }

  def test_macro( vals: Boolean* ): String = macro testMacro
  
  /**
   * macro to test stuff
   */
  def testMacro( c: MacroCtxt )( vals: c.Expr[Boolean]* ): c.Expr[String] =
    {
      import c.universe._

        def getExpressionFromConstantString2( input: String ): c.Expr[String] =
          {
            c.Expr[String]( Literal( Constant( input ) ) )
          }

      getExpressionFromConstantString2( "" )
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
        // there defined here to use the macro context

        /**
         * just a helper to test a variable (macro parameter) against a list of translators
         * the function returns the first 'match' in the list
         */
        def useTranslators( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, c.Expr[Any => String] )] = AbstractTranslator.useTranslators( c )( input, translators )

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
        def getExpressionApply( f: c.Expr[Any => String], arg: c.Expr[Any] ): c.Expr[String] =
          {
            c.Expr[String]( q"$f($arg)" )
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
                        Right( Tuple4( sl_val_name,
                          sl_val_type,
                          js_val_name,
                          getExpressionApply( translation_expression, vals.head ) ) +: list )
                      }
                  }
                }
            }
          }

      // the order of the checks is importent
      //TODO fix that later
      val preludeTranslators: Seq[AbstractTranslator] = Seq(
        new RealTranslator,
        new IntTranslator,
        new CharTranslator,
        new BoolTranslator,
        new StringTranslator
      )

      //      println( "----START----------------------------------------------------------" )

      sl_code match {
        case Expr( Literal( Constant( sl ) ) ) =>
          {
            val assets_dir = "/home/norcain/workspace/sl2-demo/public/sl/"
            val generator_dir = "generated/"
            val module_name = "test"
            //val module_name = source_file + "." + source_line
            val filename = generator_dir + module_name + ".sl"
            val placeholder = "$s"
            //val context = md5( source_file + source_line )

            // hack to remove all placeholders in comments
            var sl_code = sl.toString().replaceAll( "(?m)^--.*$", "--" );

            // check number of placeholders and given parameters
            val number_of_placeholders = countSubstring( sl_code, placeholder )
            val test = number_of_placeholders - vals.length

            if ( test > 0 )
              c.abort( c.enclosingPosition, "There are more placeholders in the sl code then parameter!" )
            else {
              if ( test < 0 )
                c.warning( c.enclosingPosition, "There are more paremter given then placeholders in the sl code!" );
            }

            // trying to transform the macro parameter to sl
            val handleResult = handleScalaValues( 0,
              number_of_placeholders,
              vals,
              preludeTranslators ++ loadTranslatorsFromDependencies( sl_code.replaceAll( Pattern.quote( placeholder ), "{| |}" ) )
            )

            if ( handleResult.isLeft ) {
              // error handling
              c.abort( c.enclosingPosition, handleResult.left.get )
            }
            else {

              // adding sl value definitions for the macro parameter
              sl_code = sl_code + "\n\n-- Generated Methods for scala values --\n"
              val paramList = handleResult.right.get

              for ( param <- paramList ) {
                sl_code = ( sl_code.replaceFirst( Pattern.quote( placeholder ), param._1 ) ) + renderSLDef( param._1, param._2, param._3 )
              }

              // write sl to tmp file
              writeSlToFile( assets_dir + filename, sl_code )

              //              println( "----END------------------------------------------------------------" )

              //build config
              val config: Config = Config( new File( assets_dir ) // source path
              , filename :: List() // sources
              , new File( assets_dir ) // class path
              , "" // mainName
              , new File( "" ) // mainParent
              , new File( assets_dir ) // destination
              , true // compile for play framework
              )

              // compile sl code
              val res = run( config )

              if ( res.isLeft ) {
                // handle compilation error
                c.abort( c.enclosingPosition, errorMessage( res.left.get ) )
              }
              else {
                val require = getExpressionFromConstantString( renderRequire( module_name ) + paramList.foldLeft( "\n// transformed scala variables \n" )( ( str, param_info ) => str + renderJSDef( param_info._3 ) ) )

                val expr_list = paramList.map( ( x ) => x._4.tree ).toList

                c.Expr[String]( q"{import de.tuberlin.uebb.sl2.impl._; $require.format (..$expr_list) }" )
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
   * Analysis the sl code for Import statements and loads the appropriate Translator classes
   *
   * @param source_code sl code
   */
  def loadTranslatorsFromDependencies( source_code: String ): Seq[AbstractTranslator] =
    {
      import de.tuberlin.uebb.sl2.slmacro.variabletranslation

      val ast = parseAst( source_code )

      ast match {
        case Right( Program( imports, _, _, _, _, _ ) ) =>
          for (
            imp <- imports if ( imp.isInstanceOf[QualifiedImport] );
            trans <- AbstractModulTranslator.resolveImport( imp.path, imp.asInstanceOf[QualifiedImport].name )
          ) yield trans
        case _ => Seq()
      }
    }

  /**
   * helper returning sl code for a constant function
   */
  def renderSLDef( sl_val_name: String, sl_val_type: String, js_val_name: String ): String =
    { f"\n\nFUN $sl_val_name%s : $sl_val_type%s\nDEF EXTERN $sl_val_name%s = {| sl['$js_val_name%s'] |}" }

  /**
   * helper returning js of a variable definition
   */
  def renderJSDef( js_val_name: String ): String =
    { f"\nsl['$js_val_name%s'] = %%s;" }

  /**
   * calculates the md5 hash of a string
   */
  def md5( input: String ): String =
    {
      val ar = MessageDigest.getInstance( "MD5" ).digest( input.getBytes() )

      ar.foldLeft( "" )( ( x, y ) => x + String.format( "%x", new Integer( y & 0xff ) ) )
    }

  /**
   * writes a String to a certain file. Deletes the file if it already exists.
   *
   * TODO: check if we can write the file first
   *
   * @param filepath eg. /tmp/foo.sl
   * @param content the content of the file
   */
  def writeSlToFile( filepath: String, content: String ): Unit =
    {
      val target_file = new File( filepath );

      if ( target_file.exists() )
        target_file.delete()

      val tmp_file: Output = Resource.fromFile( filepath )

      tmp_file.write( content )( Codec.UTF8 )
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
  def renderRequire( module_name: String ): String =
    {
      String.format(
        """
require
	( [ "generated/%s.sl" ]
	, function (foo)
		{
			foo.$main()
		}
	);

""", module_name
      )
    }

  /**
   * counts all occurrences of a substring in a string
   *
   * @see http://rosettacode.org/wiki/Count_occurrences_of_a_substring#Scala
   */
  def countSubstring( haystack: String, needle: String ): Int = {
      @tailrec def count( pos: Int, c: Int ): Int = {
        val idx = haystack indexOf ( needle, pos )
        if ( idx == -1 )
          c
        else
          count( idx + needle.size, c + 1 )
      }
    count( 0, 0 )
  }

}
