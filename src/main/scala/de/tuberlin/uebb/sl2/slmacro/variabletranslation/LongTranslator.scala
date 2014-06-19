package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._
import scala.math.BigInt

object LongTranslator {
  def scalaToJsInt( i: Any ): JValue =
    {
      JInt( BigInt( i.asInstanceOf[Long] ) )
    }

  def jsToScalaInt( input: JValue ): Long =
    {
      input match {
        case JInt( x ) => {
          if ( x.isValidLong ) // check if the input is in long range
            x.longValue()
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class LongTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Long].=:=( input ) ) {
        Some( (
          "Int",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslator.scalaToJsInt } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslator.jsToScalaInt } )
        ) )
      }
      else
        None
    }
}
