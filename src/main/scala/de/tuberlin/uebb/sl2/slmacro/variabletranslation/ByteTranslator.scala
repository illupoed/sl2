package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._
import scala.math.BigInt

object ByteTranslator {
  def scalaToJsByte( i: Any ): JValue =
    {
      JInt( BigInt( i.asInstanceOf[Byte] ) )
    }

  def jsToScalaByte( input: JValue ): Byte =
    {
      input match {
        case JInt( x ) => {
          if ( x.isValidByte ) // check if the input is in byte range
            x.byteValue()
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class ByteTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Byte].=:=( input ) ) {
        Some( (
          "Int",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslator.scalaToJsByte } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslator.jsToScalaByte } )
        ) )
      }
      else {
        None
      }
    }
}
