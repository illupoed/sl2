package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import org.json4s._

object Tuple2Translator {
  def scalaToJsTuple2( input: Any, f: Any => JValue, g: Any => JValue ): JValue =
    {
      import org.json4s._

      input match {
        case Tuple2( x, y ) => {
          val tmp: List[( String, JValue )] = List( "_cid" -> JInt( 0 ), "_var0" -> f( x ), "_var1" -> g( y ) )
          JObject( tmp )
        }
        case _ =>
          // this should never happen. This can only occur if there is a bug in handleParameter
          throw new IllegalArgumentException
      }
    }

  def jsToScalaTuple2[A, B]( input: JValue, f: JValue => A, g: JValue => B ): Tuple2[A, B] =
    {
      input match {
        case JObject( x ) => {
          val cid = x.find( j => ( j._1 == "_cid" ) )
          val var0 = x.find( j => ( j._1 == "_var0" ) )
          val var1 = x.find( j => ( j._1 == "_var1" ) )

          if ( var0.isDefined &&
            cid.isDefined &&
            var1.isDefined &&
            AbstractModulTranslator.isIntAndHasValue( cid.get._2, 0 ) ) {
            Tuple2( f( var0.get._2 ), g( var1.get._2 ) )
          }
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

case class Tuple2Translator( override val module_alias: String = "Pair" ) extends AbstractModulTranslator( module_alias ) {
  val import_path = "std/pair"

  override def translate( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      val tuple2_class_symbol: ClassSymbol = typeOf[Tuple2[_, _]].typeSymbol.asClass
      val first_type_parameter: Type = tuple2_class_symbol.typeParams( 0 ).asType.toType
      val second_type_parameter: Type = tuple2_class_symbol.typeParams( 1 ).asType.toType
      val tuple2_any_type: Type = typeOf[Tuple2[Any, Any]]

      if ( input.<:<( tuple2_any_type ) && typeOf[Tuple2[Nothing, Nothing]].<:<( input ) ) {
        val actual_type_first = first_type_parameter.asSeenFrom( input, tuple2_class_symbol )
        val actual_type_second = second_type_parameter.asSeenFrom( input, tuple2_class_symbol )

        AbstractTranslator.useTranslators( context )( actual_type_first, translators ) match {
          case Some( ( sl_type_first, imports_first, expr_s2j_first, expr_j2s_first ) ) => {
            AbstractTranslator.useTranslators( context )( actual_type_second, translators ) match {
              case Some( ( sl_type_second, imports_second, expr_s2j_second, expr_j2s_second ) ) => {
                //TODO find something more performant then reify(foo.splice). this eats up the heap very very fast
                // q"foo" is not possible because you can not use functions (Expr[Any => String]) in parameter lists
                val scala2js = reify( { ( i: Any ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.Tuple2Translator.scalaToJsTuple2( i, expr_s2j_first.splice, expr_s2j_second.splice ) } )
                val js2scala = reify( { ( i: JValue ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.Tuple2Translator.jsToScalaTuple2( i, expr_j2s_first.splice, expr_j2s_second.splice ) } )
                Some( ( f" ($module_alias%s.Pair $sl_type_first%s $sl_type_second%s) ", imports_first ++ imports_second + module_import, scala2js, js2scala ) )
              }
              case None =>
                None
            }
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