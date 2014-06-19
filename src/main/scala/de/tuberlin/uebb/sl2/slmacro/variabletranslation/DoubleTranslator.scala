package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object DoubleTranslator {
  def scalaToJsReal( i: Any ): JValue = JDouble( i.asInstanceOf[Double] )

  def jsToScalaReal( input: JValue ): Double =
    {
      input match {
        case JDouble( x ) => x
        case JInt( x ) => {
          if ( x.isValidDouble )
            x.toDouble
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class DoubleTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Double].=:=( input ) ) {
        Some( (
          "Real",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslator.scalaToJsReal } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslator.jsToScalaReal } )
        ) )
      }
      else
        None
    }
}
