package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }
import org.json4s._

object BooleanTranslator {
  def scalaToJsBool( input: Any ): JValue = JBool( input.asInstanceOf[Boolean] )

  def jsToScalaBool( input: JValue ): Boolean =
    {
      input match {
        case JBool( x ) => x
        case _ => throw new IllegalArgumentException
      }
    }
}

class BooleanTranslator extends AbstractTranslator {

  override def translate( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, Set[String], context.Expr[Any => JValue], context.Expr[JValue => Any] )] =
    {
      import context.universe._

      //      println("----BOOL-------------------------------------")
      //      println(input.actualType.=:=(typeOf[Boolean]))
      //      println(input.actualType.<:<( typeOf[Boolean] ))
      //      println(input)
      //      println(input.actualType)
      //      println(input.staticType)

      if ( input.=:=( typeOf[Boolean] ) ) {
        Some( (
          "Bool",
          Set(module_import()),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator.scalaToJsBool } ),
          reify( { de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator.jsToScalaBool } )
        ) )
      }
      else
        None
    }
}
