package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object CharTranslator {
  def scalaToJsChar( input: Any ): JValue = JString( input.asInstanceOf[Char].toString )

  def jsToScalaChar( input: JValue ): Char =
    {
      input match {
        case JString( x ) => {
          if ( x.length() == 1 )
            x.charAt( 0 )
          else
            throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }
    }
}

class CharTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      if ( typeOf[Char].=:=( input ) ) {
        Some( (
          "Char",
          Set( module_import() ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.CharTranslator.scalaToJsChar } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.CharTranslator.jsToScalaChar } )
        ) )
      }
      else
        None
    }
}
