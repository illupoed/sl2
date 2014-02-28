package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std._


object AbstractModulTranslator{
  def resolveImport(path : String, name: String) : Option[AbstractModulTranslator] =
  {
    path match 
    {
      case "std/option" => 
        Some(new OptionTranslator(name))
      case _ => None
    }
  }
}

abstract class AbstractModulTranslator(val module_alias: String) extends AbstractTranslator {

 
}