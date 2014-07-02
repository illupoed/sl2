package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object FloatTranslator {
  def scalaToJsReal( i: Any ): JValue = JDouble( i.asInstanceOf[Float] )

  def jsToScalaReal( input: JValue ): Float =
    {
      input match {
        case JDouble( x ) => {
          if ( ( x <= Float.MaxValue && x >= Float.MinValue ) || x.isNaN() || x.isInfinity )
            x.toFloat
          else
            throw new IllegalArgumentException
        }
        case JInt( x ) => {
          if ( x.isValidFloat )
            x.toFloat
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class FloatTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Float].=:=( input ) ) {
        Some( (
          "Real",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslator.scalaToJsReal } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslator.jsToScalaReal } )
        ) )
      }
      else
        None
    }
}
