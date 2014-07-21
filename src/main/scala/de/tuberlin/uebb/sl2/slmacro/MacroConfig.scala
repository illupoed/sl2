package de.tuberlin.uebb.sl2.slmacro

trait MacroConfig {

  /**
   * path where play finds its sl assets
   */
  val assets_dir = "/home/norcain/workspace/sl2-demo/public/sl/"

  /**
   * sub path of assets_dir, where the files of the sl inline code compiler will be created
   */
  val inline_sl_macro_folder = "generated_inline/"

  /**
   * sub path of assets_dir, where the @sl_function annotation macro puts its files
   */
  val annotation_sl_macro_folder = "generated_annotation/"

  /**
   * shape of placeholder in sl code for scala variables
   */
  val scala_var_placeholder = "#s"

  /**
   * default directory separator
   */
  val directory_seperator = '/'

  /**
   * the play framework will listen under this uri for sl function calls to scala functions
   */
  val inline_sl_macro_handler_uri = "/ajax"

}