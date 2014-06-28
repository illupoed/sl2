package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._
import scala.math.BigInt

object ShortTranslator {
  def scalaToJsShort( i: Any ): JValue =
    {
      JInt( BigInt( i.asInstanceOf[Short] ) )
    }

  def jsToScalaShort( input: JValue ): Short =
    {
      input match {
        case JInt( x ) => {
          if ( x.isValidShort ) // check if the input is in short range
            x.shortValue()
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class ShortTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Short].=:=( input ) ) {
        Some( (
          "Int",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslator.scalaToJsShort } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslator.jsToScalaShort } )
        ) )
      }
      else {
        None
      }
    }
}
