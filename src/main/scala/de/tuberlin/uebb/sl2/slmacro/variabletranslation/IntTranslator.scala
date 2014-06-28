package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._
import scala.math.BigInt

object IntTranslator {
  def scalaToJsInt( i: Any ): JValue =
    {
      JInt( BigInt( i.asInstanceOf[Int] ) )
    }

  def jsToScalaInt( input: JValue ): Int =
    {
      input match {
        case JInt( x ) => {
          if ( x.isValidInt ) // check if the input is in integer range
            x.intValue()
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class IntTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Int].=:=( input ) ) {
        Some( (
          "Int",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator.scalaToJsInt } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator.jsToScalaInt } )
        ) )
      }
      else {
        None
      }
    }
}
