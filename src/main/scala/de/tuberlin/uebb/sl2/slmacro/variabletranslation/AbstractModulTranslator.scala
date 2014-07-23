package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std._

object AbstractModulTranslator {

  /**
   * returns for an sl module (depending on its path) all 
   */
  def resolveImport( path: String, name: String ): Seq[AbstractModulTranslator] =
    {
      val translators = allModulTranslators()

      for ( t <- translators if ( t.import_path == path ) ) yield { t.rename(name) }
    }

  /**
   * returns all Translators for Modules
   *
   * TODO in the future this should be done by reflection | get all Classes which extend AbstractTranslator
   * 
   * TODO get all subclasses (die keine abstrakten Klassen sind)
   */
  def allModulTranslators(): Seq[AbstractModulTranslator] = Seq( new OptionTranslator(), new SeqTranslator(), new EitherTranslator(), new Tuple2Translator())
}

/**
 *  
 * 
 * @param module_alias all subclasses have to define a default for module_alias
 * Attention: translators which translate data types in the same module must have the same module_alias!!!
 * eg.: the module foo.sl defines the public types Bar and Baz then you have to create the translators like 
 * BarTranslator( override val module_alias: String = "Foo" ) and BazTranslator( override val module_alias: String = "Foo" ) 
 * 
 * TODO complete the comments
 */
abstract class AbstractModulTranslator( override val module_alias: String ) extends AbstractTranslator {

  override final def module_import(): String = {
    f"""IMPORT "$import_path%s" AS $module_alias%s"""
  }

  /**
   * the import path of the module which will be translated
   */
  val import_path: String

  def rename( module_alias: String ): AbstractModulTranslator
}