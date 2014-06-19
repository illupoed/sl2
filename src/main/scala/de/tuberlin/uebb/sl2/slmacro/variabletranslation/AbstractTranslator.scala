package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object AbstractTranslator {

  /**
   * Main function to use the translator classes. Gets to a given scala type and a list of translators classes
   * all needed informations to translate a value form scala to sl and the other way round
   */
  def useTranslators( c: MacroCtxt )( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], c.Expr[Any => JValue], c.Expr[JValue => Any] )] =
    {
      if ( input.<:<( c.universe.typeOf[Null] )
        || input.<:<( c.universe.typeOf[Nothing] ) )
        return None

        def helper( input: c.universe.Type, translators_rest: Seq[AbstractTranslator] ): Option[( String, Set[String], c.Expr[Any => JValue], c.Expr[JValue => Any] )] =
          {
            if ( translators_rest.isEmpty )
              None
            else {
              val foo = translators_rest.head.translate( c )( input, translators )
              if ( foo.isDefined )
                foo
              else
                helper( input, translators_rest.tail )
            }

          }

      helper( input, translators )
    }

  /**
   * a wrapper for useTranslator
   */
  def useTranslatorSLToScala( c: MacroCtxt )( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], c.Expr[JValue => Any] )] =
    {
      useTranslators( c )( input, translators ) match {
        case Some( ( sl_type, import_statements, _, fun_sl_to_scala ) ) => Some( ( sl_type, import_statements, fun_sl_to_scala ) )
        case None => None
      }
    }

  /**
   * a wrapper for useTranslator
   */
  def useTranslatorScalaToSL( c: MacroCtxt )( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], c.Expr[Any => JValue] )] =
    {
      useTranslators( c )( input, translators ) match {
        case Some( ( sl_type, import_statements, fun_scala_to_sl, _ ) ) => Some( ( sl_type, import_statements, fun_scala_to_sl ) )
        case None => None
      }
    }

  /**
   * Returns all translators for datatypes defined in the sl prelude
   */
  def preludeTranslators(): Seq[AbstractTranslator] = Seq(
    new FloatTranslator,
    new DoubleTranslator,
    new IntTranslator,
    new LongTranslator,
    new CharTranslator,
    new BooleanTranslator,
    new StringTranslator,
    new UnitTranslator
  )

}

abstract class AbstractTranslator {

  /**
   * This function searches in a given compiler context, a set of translators and a given
   * scala type for the sl equivalent and returns the AST's of some helper functions
   *
   * @param context the scala (compile time) macro context
   * @param input a input type
   * @param translators a fixed list of translators (at the moment is the order important)
   * @return Option(
   *   ( sl type
   *   , sl import strings
   *   , function to translate a scala value of type 'input' to the JSON representation of the sl value
   *   , function to translate the JSON representation of a sl value of type 'sl type' to a scala value of type 'input'
   *   ))
   */
  def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )]

  def module_alias(): String = ""

  def module_import(): String = ""
}