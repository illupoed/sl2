package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }

class BoolTranslator extends AbstractTranslator {

  override def handleParameter( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, context.Expr[Any => String] )] =
    {
      import context.universe._

      //      println("----BOOL-------------------------------------")
      //      println(input.actualType.=:=(typeOf[Boolean]))
      //      println(input.actualType.<:<( typeOf[Boolean] ))
      //      println(input)
      //      println(input.actualType)
      //      println(input.staticType)

      if ( input.<:<( typeOf[Boolean] ) ) {
        Some( ( "Bool", reify( { ( i: Any ) => "%s".format( i ) } ) ) )
      }
      else
        None
    }
}