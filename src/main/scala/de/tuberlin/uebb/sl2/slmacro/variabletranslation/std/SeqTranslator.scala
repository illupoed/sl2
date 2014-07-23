package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import org.json4s._

object SeqTranslator {
  def scalaToJsSeq( input: Any, f: Any => JValue ): JValue =
    {
      import org.json4s._

      input match {
        case Seq( x, xs @ _* ) => {
          val tmp: List[( String, JValue )] = List( "_cid" -> JInt( 1 ), "_var0" -> f( x ), "_var1" -> scalaToJsSeq( xs, f ) )
          JObject( tmp )
        }
        case Seq() => JInt( 0 )
        case _ =>
          // this should never happen. This can only occur if there is a bug in handleParameter
          throw new IllegalArgumentException
      }
    }

  def jsToScalaSeq[T]( input: JValue, f: JValue => T ): Seq[T] =
    {
      input match {
        case JInt( _ ) => Seq(): Seq[T]
        case JObject( x ) => {
          val seq_head = x.find( j => ( j._1 == "_var0" ) )
          val seq_tail = x.find( j => ( j._1 == "_var1" ) )
          if ( seq_head.isDefined && seq_tail.isDefined )
            Seq( f( seq_head.get._2 ) ) ++ jsToScalaSeq[T]( seq_tail.get._2, f )
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

case class SeqTranslator( override val module_alias: String = "SeqList" ) extends AbstractModulTranslator( module_alias ) {
  val import_path = "std/list"

  override def translate( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      val seq_class_symbol: ClassSymbol = typeOf[Seq[_]].typeSymbol.asClass
      val first_type_parameter: Type = seq_class_symbol.typeParams( 0 ).asType.toType
      val seq_any_type: Type = typeOf[Seq[Any]]
      
      if ( input.<:<( seq_any_type ) && typeOf[Seq[Nothing]].<:<( input ) ) {
        val actual_type = first_type_parameter.asSeenFrom( input, seq_class_symbol )

        AbstractTranslator.useTranslators( context )( actual_type, translators ) match {
          case Some( ( sl_type, imports, expr_s2j, expr_j2s ) ) =>
            {
              //TODO find something more performant then reify(foo.splice). this eats up the heap very very fast
              // q"foo" is not possible because you can not use functions (Expr[Any => String]) in parameter lists
              val scala2js = reify( { ( i: Any ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.SeqTranslator.scalaToJsSeq( i, expr_s2j.splice ) } )
              val js2scala = reify( { ( i: JValue ) => de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.SeqTranslator.jsToScalaSeq( i, expr_j2s.splice ) } )
              Some( ( f" ($module_alias%s.List $sl_type%s) ", imports + module_import, scala2js, js2scala ) )
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