package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import org.json4s._

object OptionTranslator {
  def scalaToJsOption( input: Any, f: Any => JValue ): JValue =
    {
      import org.json4s._

      input match {
        case Some( x ) => {
          val tmp: List[( String, JValue )] = List( "_cid" -> JInt( 0 ), "_var0" -> f( x ) )
          JObject( tmp )
        }
        case None => JInt( 1 )
        case _ =>
          // this should never happen. This can only occur if there is a bug in handleParameter
          throw new IllegalArgumentException
      }
    }

  def jsToScalaOption[T]( input: JValue, f: JValue => T ): Option[T] =
    {
      input match {
        case JInt( _ ) => None: Option[T]
        case JObject( x ) => {
          val tmp = x.find( j => ( j._1 == "_var0" ) )
          if ( tmp.isDefined )
            Some( f( tmp.get._2 ) )
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

case class OptionTranslator( override val module_alias: String = "Opt" ) extends AbstractModulTranslator( module_alias ) {
  val import_path = "std/option"

  override def translate( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      val option_class_symbol: ClassSymbol = typeOf[Option[_]].typeSymbol.asClass
      val first_type_parameter: Type = option_class_symbol.typeParams( 0 ).asType.toType
      val option_any_type: Type = typeOf[Option[Any]]

      if ( input.<:<( option_any_type ) ) {
        val actual_type = first_type_parameter.asSeenFrom( input, option_class_symbol )

        AbstractTranslator.useTranslators( context )( actual_type, translators ) match {
          case Some( ( sl_type, imports, expr_s2j, expr_j2s ) ) =>
            {
              //TODO find something more performant then reify(foo.splice). this eats up the heap very very fast
              // q"foo" is not possible because you can not use functions (Expr[Any => String]) in parameter lists
              val scala2js = reify( { ( i: Any ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.OptionTranslator.scalaToJsOption( i, expr_s2j.splice ) } )
              val js2scala = reify( { ( i: JValue ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.OptionTranslator.jsToScalaOption( i, expr_j2s.splice ) } )
              Some( ( module_alias + ".Option ( " + sl_type + " )", imports + module_import, scala2js, js2scala ) )
            }
          case None =>
            None
        }
      }
      else
        None
    }

  override def rename( module_alias: String ) = copy( module_alias );

}