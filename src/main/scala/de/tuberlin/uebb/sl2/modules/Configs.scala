package de.tuberlin.uebb.sl2.modules

import java.io.File

trait Configs {
  case class Config(
    /**
     * where to look for source files
     */
    val sourcepath: File,
    /**
     * which source files at source path to compile
     */
    val sources: List[String],
    /**
     * where to look for compiled versions of imported modules
     * (this should usually be the destination directory.
     * if its not, manual changes to the requirejs-config might
     * be needed.)
     */
    val classpath: File,
    /**
     * the simple name of the main file compiled
     */
    val mainName: String,
    /**
     * the parent directory of the main file compiled
     */
    val mainParent: File,
    /**
     * where to put the compiled files from source.
     */
    val destination: File,
    /**
     * should the multidriver create an html file to execute the sl code in a browser or via nodejs
     * the MultiDriver should still copy library files to the classpath folder
     * 
     * this option is only used by the MultiDriver/MacroDriver
     */
    val generate_index_html: Boolean = true,
    /**
     * if this is set to true all files in sources should contain a main function
     * 
     * this field is only used by the MultiDriver/MacroDriver
     */
    val main_function_is_required: Boolean = false
    )
}
