package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object StringTranslator {
  def scalaToJsString( i: Any ): JValue = JString( i.asInstanceOf[String] )

  def jsToScalaString( input: JValue ): String =
    {
      input match {
        case JString( x ) => x
        case _ => throw new IllegalArgumentException
      }
    }
}

class StringTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( input.=:=( typeOf[String] ) ) {
        Some( ( 
            "String",
            Set(module_import()),
            reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.StringTranslator.scalaToJsString } ),
            reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.StringTranslator.jsToScalaString } )
        ) )
      }
      else
        None
    }

}
