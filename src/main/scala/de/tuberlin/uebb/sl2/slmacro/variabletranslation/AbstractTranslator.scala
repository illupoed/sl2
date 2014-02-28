package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import reflect.macros.{ Context => MacroCtxt }

object AbstractTranslator {

  /**
   * just a helper to test a variable (macro parameter) against a list of translators
   * the function returns the first 'match' in the list
   */
  def useTranslators( c: MacroCtxt )( input: c.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, c.Expr[Any => String] )] =
    {
      if ( input.<:<( c.universe.typeOf[Null] )
        || input.<:<( c.universe.typeOf[Nothing] ) )
        return None

        def helper( input: c.universe.Type, translators_rest: Seq[AbstractTranslator] ): Option[( String, c.Expr[Any => String] )] =
          {
            if ( translators_rest.isEmpty )
              None
            else {
              val foo = translators_rest.head.handleParameter( c )( input, translators )
              if ( foo.isDefined )
                foo
              else
                helper( input, translators_rest.tail )
            }

          }

      helper( input, translators )

    }

}

abstract class AbstractTranslator {

  def handleParameter( context: MacroCtxt )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, context.Expr[Any => String] )]
}