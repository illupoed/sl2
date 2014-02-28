package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }

class StringTranslator extends AbstractTranslator {

  override def handleParameter(context: MacroCtxt)( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, context.Expr[Any => String] )] =
    {
      import context.universe._

      if ( input.<:<( typeOf[String] ) ) {
        Some( ( "String", reify( {(i:Any) => "'%s'".format( i )} ) ) )
      }
      else
        None
    }

}