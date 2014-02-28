package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractModulTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.AbstractTranslator
import scala.reflect.macros.Context
import scala.reflect.macros.Universe

object OptionTranslator {
  def scalaToJsOption( f: Any => String, input: Any ): String =
    {
      input match {
        case Some( x ) => "{_cid: 0, _var0 : %s}".format(f( x ))
        case None => "1"
        case _ => 
          // this should never happen. This can only occure if there is a bug in handleParameter
          throw new IllegalArgumentException
      }

    }
}

class OptionTranslator( override val module_alias: String ) extends AbstractModulTranslator( module_alias ) {

  def handleParameter( context: Context )( input: context.universe.Type, translators: Seq[AbstractTranslator] ): Option[( String, context.Expr[Any => String] )] =
    {
      import context.universe._

      val option_class_symbol: ClassSymbol = typeOf[Option[_]].typeSymbol.asClass
      val first_type_parameter: Type = option_class_symbol.typeParams( 0 ).asType.toType

      if ( input.<:<( typeOf[Option[Any]] ) ) {
        val actual_type = first_type_parameter.asSeenFrom( input, option_class_symbol )
        
        println(actual_type)
        
        AbstractTranslator.useTranslators( context )( actual_type, translators ) match {
          case Some( ( sl_type, expr ) ) =>
            {
              //TODO find something more performant then reify(foo.splice). this eats up the heap very very fast
              // q"foo" is not possible because you can not use functions (Expr[Any => String]) in parameter lists
              val tmp = reify({import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std; (i:Any) => OptionTranslator.scalaToJsOption( expr.splice, i)})
              Some( ( module_alias + ".Option ( " + sl_type + " )", tmp ) )
            }
          case None =>
            None
        }
      }
      else
        None
    }

}