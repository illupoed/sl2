package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import org.json4s._

object EitherTranslator {
  def scalaToJsEither( input: Any, f: Any => JValue, g: Any => JValue ): JValue =
    {
      import org.json4s._

      input match {
        case Left( x ) => {
          val tmp: List[( String, JValue )] = List( "_cid" -> JInt( 0 ), "_var0" -> f( x ) )
          JObject( tmp )
        }
        case Right( x ) => {
          val tmp: List[( String, JValue )] = List( "_cid" -> JInt( 1 ), "_var0" -> g( x ) )
          JObject( tmp )
        }
        case _ =>
          // this should never happen. This can only occur if there is a bug in handleParameter
          throw new IllegalArgumentException
      }
    }

  def jsToScalaEither[A, B]( input: JValue, f: JValue => A, g: JValue => B ): Either[A, B] =
    {
      input match {
        case JObject( x ) => {
          val cid = x.find( j => ( j._1 == "_cid" ) )
          val var0 = x.find( j => ( j._1 == "_var0" ) )
          if ( var0.isDefined && cid.isDefined ) {
            if ( cid.get._2 == 0 )
              return Left( f( var0.get._2 ) )
            if ( cid.get._2 == 1 )
              return Right( g( var0.get._2 ) )
            else
              throw new IllegalArgumentException
          }
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

case class EitherTranslator( override val module_alias: String = "Eith" ) extends AbstractModulTranslator( module_alias ) {
  val import_path = "std/either"

  override def translate( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      val either_class_symbol: ClassSymbol = typeOf[Either[_, _]].typeSymbol.asClass
      val first_type_parameter: Type = either_class_symbol.typeParams( 0 ).asType.toType
      val second_type_parameter: Type = either_class_symbol.typeParams( 1 ).asType.toType
      val either_any_type: Type = typeOf[Either[Any, Any]]

      if ( input.<:<( either_any_type ) ) {
        val actual_type_first = first_type_parameter.asSeenFrom( input, either_class_symbol )
        val actual_type_second = second_type_parameter.asSeenFrom( input, either_class_symbol )

        AbstractTranslator.useTranslators( context )( actual_type_first, translators ) match {
          case Some( ( sl_type_first, imports_first, expr_s2j_first, expr_j2s_first ) ) => {
            AbstractTranslator.useTranslators( context )( actual_type_second, translators ) match {
              case Some( ( sl_type_second, imports_second, expr_s2j_second, expr_j2s_second ) ) => {
                //TODO find something more performant then reify(foo.splice). this eats up the heap very very fast
                // q"foo" is not possible because you can not use functions (Expr[Any => String]) in parameter lists
                val scala2js = reify( { ( i: Any ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.EitherTranslator.scalaToJsEither( i, expr_s2j_first.splice, expr_s2j_second.splice ) } )
                val js2scala = reify( { ( i: JValue ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.EitherTranslator.jsToScalaEither( i, expr_j2s_first.splice, expr_j2s_second.splice ) } )
                Some( ( f" ($module_alias%s.Either $sl_type_first%s $sl_type_second%s) ", imports_first ++ imports_second + module_import, scala2js, js2scala ) )
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