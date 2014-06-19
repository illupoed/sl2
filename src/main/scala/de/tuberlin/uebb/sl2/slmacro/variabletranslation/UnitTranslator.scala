package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._
import scala.reflect.macros.Context

object UnitTranslator {
  def scalaToJsVoid( input: Any ): JValue = JInt( 0 )

  def jsToScalaVoid( input: JValue ): Unit =
    {
      input match {
        case JInt( x ) if (x == 0) => Unit
        case _ => throw new IllegalArgumentException
      }
    }
}

class UnitTranslator extends AbstractTranslator {

  def translate( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( input.=:=( typeOf[Unit] ) ) {
        Some( (
            "Void",
            Set(module_import()),
            reify( { import de.tuberlin.uebb.sl2.slmacro.variabletranslation; UnitTranslator.scalaToJsVoid } ),
            reify( { import de.tuberlin.uebb.sl2.slmacro.variabletranslation; UnitTranslator.jsToScalaVoid } )
            )
        )
      }
      else
        None
    }

}