package de.tuberlin.uebb.sl2.slmacro

import scala.reflect.macros.{ Context => MacroCtxt }
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe.Flag._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation._
import de.tuberlin.uebb.sl2.modules._
import de.tuberlin.uebb.sl2.impl._
import de.tuberlin.uebb.sl2.modules.Syntax.VarFirstClass
import java.io.File
import org.json4s._
import org.json4s.native.JsonMethods._
import scala.collection.mutable.HashMap

class sl_function extends StaticAnnotation {
  def macroTransform( annottees: Any* ) = macro sl_function.impl
}

object sl_function
    extends CombinatorParser
    with MultiDriver
    with Configs
    with AbstractFile
    with CodeGenerator
    with Syntax
    with ProgramChecker
    with JsSyntax
    with Errors
    with SignatureSerializer
    with DebugOutput
    with ModuleResolver
    with ModuleNormalizer
    with ModuleLinearization
    with PreProcessing
    with NameSupply
    with Graph[VarFirstClass]
    with Context
    with Type
    with SyntaxTraversal
    with EnrichedLambdaCalculus
    with Substitution
    with Unification
    with MacroConfig

    // Implementations 
    with GraphImpl[VarFirstClass]
    with DTCheckerImpl
    with FDCheckerImpl
    with TypeCheckerImpl
    with ProgramCheckerImpl
    with LetRecSplitter

    with ModuleResolverImpl
    with ModuleNormalizerImpl
    with ModuleContextImpl
    with SignatureJsonSerializer {

  private val visitedObjects: HashMap[String, Int] = new HashMap()

  private val simple_date_format = new java.text.SimpleDateFormat( "dd-MM-yyyy" )

  /**
   * returns the type to a tree representing a type
   *
   * pretty dirty hack proposed by Eugene Burmako
   * @see https://stackoverflow.com/questions/19379436/cant-access-parents-members-while-dealing-with-macro-annotations?answertab=votes#tab-top
   */
  def getTypeFromTypeTree( c: MacroCtxt )( tree: c.Tree ): Option[c.Type] = {

    if ( tree.isEmpty )
      None
    else {
      import c.universe._
      val tmp = tree.duplicate

      Some( c.typeCheck( q"{7.asInstanceOf[$tmp]}" ).tpe )
    }
  }

  /**
   * this function tests if a scala function is well shaped
   *
   * There 2 main reasons:
   *  - the name of the function has to be a valid name for a sl function
   *  - it also has to be a valid filename on all platforms (thats why its pretty strict) // this should be changed later
   */
  def isAllowedFunctionName( input: String ): Boolean = input.matches( "^[a-z0-9]+$" )

  /**
   * this function tests if a given class path (package_path.object_name) is well shaped
   *
   * Attention: this check is very lazy and not complete (eg. a class path can not start with a number)
   */
  def isAllowedClassPath( input: String ): Boolean = input.matches( "^[a-zA-Z0-9._]+$" )

  def impl( c: MacroCtxt )( annottees: c.Expr[Any]* ): c.Expr[Any] = {
    import c.universe._

    // ----------------------------------------------------------------------
    // ----- Helper functions -----------------------------------------------
    // there defined here to use the macro context

    /**
     * a list of all available translators
     */
    val translators = AbstractTranslator.preludeTranslators() ++ AbstractModulTranslator.allModulTranslators()

      /**
       * just a wrapper for AbstractTranslator.useTranslatorSLToScala
       */
      def useTranslatorsInput( input: c.universe.Type ): Option[( String, Set[String], c.Expr[JValue => Any] )] = {
        AbstractTranslator.useTranslatorSLToScala( c )( input, translators )
      }

      /**
       * just a wrapper for AbstractTranslator.useTranslatorScalaToSL
       */
      def useTranslatorsOutput( input: c.universe.Type ): Option[( String, Set[String], c.Expr[Any => JValue] )] = {
        AbstractTranslator.useTranslatorScalaToSL( c )( input, translators )
      }

      /**
       * this function creates the AST of a wrapper function for the annotated function
       *
       * eg.:
       * Input:
       * @sl_function def foo(bar:InputType1, baz:InputType2):ResultType = ???
       * Result:
       * def foo_sl_helper(bar: JValue): JValue = ScalaToSlResultType(foo(slToScalaInputType1(bar).asInstanceOf[InputType1], slToScalaInputType2(baz).asInstanceOf[InputType2]))
       *
       * The result function will be called sl code via ajax
       *
       * @param fun_name foo in the example
       * @param param_converter the AST of the converter functions slToScalaInputType1 and slToScalaInputType2 in the example
       * @param params list of the input Types InputType1 and InputType2 in the example
       * @param result_converte the AST of the function ScalaToSlResultType in the example
       */
      def createHelperFunction( fun_name: c.universe.Name, param_converter: Seq[c.Expr[JValue => Any]], params: Seq[c.Type], result_converter: c.Expr[Any => JValue] ): c.Expr[Any] = {
        val helper_name = newTermName( fun_name.decoded + "_sl_helper" )
        var i = 0;

        val tmp: Seq[Pair[c.universe.ValDef, c.universe.Tree]] = for ( x <- param_converter.zip( params ) ) yield {
          val param_name = newTermName( "p" + i )
          i = i + 1
          val param_type = x._2
          val converter_fun = x._1

          val res = Pair(
            q"val $param_name : org.json4s.JValue",
            q"$converter_fun($param_name).asInstanceOf[$param_type]"
          )

          res
        }

        val ( a, b ) = tmp.unzip[c.universe.ValDef, c.universe.Tree]

        val tree = q"""
def $helper_name(...${List( a )}): org.json4s.JValue = {
  $result_converter($fun_name(...${List( b )}))
}
"""
        c.Expr[Any]( tree )
      }

    // ---- End of Helper Functions ------------------------------------------

    val ( fun_modifiers, fun_name, fun_params, fun_result_type_opt ) = annottees.head.tree match {
      case DefDef( t1, t2, List(), List( t3 ), t4 /* @ Ident(_)*/ , _ ) =>
        {
          ( t1, t2, t3, getTypeFromTypeTree( c )( t4 ) )
        }
      case _ => c.abort( c.enclosingPosition, "It is only possible to annotate functions! The function has to meet the following requirements:\n - No type parameter\n - One parameter list" )
    }

    for ( fun_param <- fun_params ) {
      fun_param match {
        case ValDef( mods, name, _, _ ) if mods.hasFlag( Flag.DEFAULTPARAM ) => c.warning( c.enclosingPosition, "The sl equivalent of this function can not handle default parameter thus the default value of (" + name.decoded + ") will be ignored." )
        case _ =>
      }
    }

    val fun_param_types = fun_params.map( x => ( getTypeFromTypeTree( c )( x.tpt ) ).get )

    val class_name = c.enclosingClass match {
      case ModuleDef( _, n, _ ) => n
      case _ => c.abort( c.enclosingPosition, "Function has to be static! Could not find enclosing Object." )
    }

    val fun_result_type = fun_result_type_opt match {
      case None => c.abort( c.enclosingPosition, "Function has to define result type." )
      case Some( x ) => x
    }

    if ( fun_modifiers.hasFlag( Flag.PRIVATE )
      || fun_modifiers.hasFlag( Flag.PROTECTED ) )
      c.abort( c.enclosingPosition, "Function has to be public." )

    // this case should not be possible
    if ( fun_modifiers.hasFlag( Flag.DEFERRED ) )
      c.abort( c.enclosingPosition, "Function cannot be abstract." )

    if ( !isAllowedFunctionName( fun_name.decoded ) )
      c.abort( c.enclosingPosition, "Function name should only contain small letters and numbers. Found: '" + fun_name.decoded + "'" )

    val package_path = showRaw( c.enclosingClass.symbol.owner )
    val class_path = package_path + "." + class_name;

    if ( !isAllowedClassPath( class_path ) )
      c.abort( c.enclosingPosition, "Object path should only contain letters, numbers, points and underlines. Found: '" + class_path + "'" )

    if ( isFirstInvoke( class_path ) ) {
      deleteFiles( class_path )
    }

    // ----- translate result type --------------------------------------------
    val ( fun_result_sl_type, result_type_import_statements, fun_result_scala_to_sl ) = useTranslatorsOutput( fun_result_type ) match {
      case Some( ( x, y, z ) ) => {
        ( x, y, z )
      }
      case None => c.abort( c.enclosingPosition, f"Could not find a sl equivalent for the result type ($fun_result_type%s) of function $fun_name%s()." )
    }

    // ----- translate parameter types ----------------------------------------
    val ( fun_param_sl_types, param_types_import_statements, fun_param_sl_to_scala ) = fun_param_types.foldRight( ( Seq[String](), Set[String](), Seq[c.Expr[JValue => Any]]() ) )(
      ( input_type, tmp_result ) =>
        {
          useTranslatorsInput( input_type ) match {
            case Some( ( fun_param_sl_type, import_statements, fun_param_sl_to_scala ) ) => {
              ( tmp_result._1.+:( fun_param_sl_type ), tmp_result._2.++( import_statements ), tmp_result._3.+:( fun_param_sl_to_scala ) )
            }
            case None =>
              c.abort( c.enclosingPosition, f"Could not find a sl equivalent to the parameter type ($input_type%s) of function $fun_name%s()." )
          }
        }
    )

    // option is needed for the result type -> result type is always DOM ( Option ( transformed_scala_result_type ) )
    val import_statements = ( param_types_import_statements ++ result_type_import_statements ) + ( new std.OptionTranslator().module_import )

    // ----- create file ------------------------------------------------------
    val body = defineSlFunction( fun_name.decoded, class_path, fun_param_sl_types, fun_result_sl_type )
    val file_content = defineSlFile( import_statements, body, class_path, fun_name.decoded )

    // ----- writing file -----------------------------------------------------
    createSlFile( class_path, fun_name.decoded, file_content )

    // ----- create helper function to handle calls ---------------------------
    val helper_fun = createHelperFunction(
      fun_name,
      fun_param_sl_to_scala,
      fun_param_types,
      fun_result_scala_to_sl
    )

    // set first invoke
    setFirstInvoke( class_path )

    // on success return the input and the helper function
    c.Expr[Any]( q"""
${annottees.head}
$helper_fun
    """ )
  }

  /**
   * generate sl code for scala function calls
   *
   * @param fun_name the name of the called scala function
   * @param class_path package_path + '.' + object_name of the called scala function
   * @param parameter_types sl types of the called scala function
   * @param target_type sl retrun type of the called scala function
   */
  def defineSlFunction( fun_name: String, class_path: String, parameter_types: Seq[String], target_type: String ): String =
    {
      val parameter_type_string = parameter_types.foldLeft( "" )( ( a, b ) => ( f"$a%s$b%s -> " ) )
      val sl_param_names = renderSLParameterNames( parameter_types.length, parameter_types.length )
      val sl_param_brace = renderSLParameterList( parameter_types.length, parameter_types.length )
      val sl_param_brace_async = if ( 0 == parameter_types.length ) "$callbackFun" else "$callbackFun, " + sl_param_brace

      f"""
-- this functions should call the scala function:
-- $class_path%s.$fun_name%s
PUBLIC FUN $fun_name%sSync : $parameter_type_string%s DOM ( Opt.Option ($target_type%s) )
DEF $fun_name%sSync$sl_param_names%s = {| _sendRequestSync("$inline_sl_macro_handler_uri%s", "$class_path%s", "$fun_name%s_sl_helper") ($sl_param_brace%s) |} : DOM ( Opt.Option ($target_type%s) )

PUBLIC FUN $fun_name%sAsync : ( Opt.Option ($target_type%s) -> DOM Void ) -> $parameter_type_string%s DOM Void
DEF $fun_name%sAsync callbackFun$sl_param_names%s = {| _sendRequestAsync("$inline_sl_macro_handler_uri%s", "$class_path%s", "$fun_name%s_sl_helper") ($sl_param_brace_async%s) |} : DOM Void
"""
    }

  /**
   * helper for defineSlFunction
   */
  def renderSLParameterNames( param_count: Int, i: Int ): String =
    {
      if ( i < 0 ) throw new IllegalArgumentException()

      i match {
        case 0 => ""
        case _ => " p%s%s".format( param_count - i, renderSLParameterNames( param_count, i - 1 ) )
      }
    }

  /**
   * helper for defineSlFunction
   */
  def renderSLParameterList( param_count: Int, i: Int ): String =
    {
      if ( i < 0 ) throw new IllegalArgumentException()

      if ( param_count == 0 && i == 0 ) return ""

      val prefix = if ( param_count == i ) "" else ", "

      i match {
        case 0 => ""
        case _ => {
          "%s$p%s%s".format( prefix, param_count - i, renderSLParameterList( param_count, i - 1 ) )
        }
      }
    }

  /**
   * generates the whole sl file
   *
   * @param imports qualified sl imports which are needed by the generated functions
   * @param body sl_code
   * @param class_path package_path + '.' + object_name of the called scala function
   * @param fun_name the name of the called scala function
   */
  def defineSlFile( imports: Set[String], body: String, class_path: String, fun_name: String ): String = {
    val cur_dat = simple_date_format.format( new java.util.Date() )
    val imports_str = imports.filter( ( x ) => x.length() > 0 ).foldLeft( "" )( ( a, b ) => f"$a%s\n$b%s" )
    // DO NOT CHANGE THE FIRST 3 LINES or change the function MacroDriver.getClassFunctionFromGeneratedSLFile accordingly
    f"""-- DO NOT ALTER THIS FILE! --------------------------------
-- cp: $class_path%s
-- fn: $fun_name%s
-- --------------------------------------------------------
-- this file was generated by @sl_function macro ----------
-- on $cur_dat%s ------------------------------------------
IMPORT EXTERN "std/_scalafun"
$imports_str%s
$body%s
"""
  }

  /**
   * test if this is the first invocation of the annotation macro in the current object
   *
   * if you annotate two functions in one object you have to delete the generated sl code only on the first call
   */
  def isFirstInvoke( object_path: String ): Boolean = {
    ( !visitedObjects.contains( object_path ) )
  }

  /**
   * set that you completed the first invocation of the annotation macro in the current object
   */
  def setFirstInvoke( object_path: String ) {
    visitedObjects.+=( ( object_path, 1 ) )
  }

  /**
   * deletes all files related to the current object/function
   */
  def deleteFiles( class_path: String ) {
    import scalax.file.Path

    val dir = Path.fromString( classPathToFileSystemPath( class_path ) )

    if ( dir.exists ) {
      dir.deleteRecursively( true, false );
    }
  }

  /**
   * writes the generated sl code to the file system and compiles it
   */
  def createSlFile( class_path: String, fun_name: String, body: String ): Unit = {

    val filename = fun_name + ".sl"

    writeSlToFile( classPathToFileSystemPath( class_path ) + filename, body )

    //build config
    val config: Config = Config( new File( assets_dir ) // source path
    , ( classPathToFileSystemPath( class_path, true ) + filename ) :: List() // sources
    , new File( assets_dir ) // class path
    , "" // mainName
    , new File( "" ) // mainParent
    , new File( assets_dir ) // destination
    , false // generate a html file?
    )

    // compile sl code
    val res = run( config )
  }

  /**
   * converts a class path to a file system path.
   *
   * @param class_path package_path + '.' + object_name of the scala function (eg. package.path.Object_name -> annotation_sl_macro_folder/package/path/object_name)
   */
  def classPathToFileSystemPath( class_path: String, relative_to_assets_dir: Boolean = false ): String =
    {
      if ( relative_to_assets_dir ) {
        annotation_sl_macro_folder.replace( '\\', directory_seperator ) + class_path.replace( '.', directory_seperator ).toLowerCase + directory_seperator
      }
      else {
        ( assets_dir + annotation_sl_macro_folder ).replace( '\\', directory_seperator ) + class_path.replace( '.', directory_seperator ).toLowerCase + directory_seperator
      }

    }

  /**
   * writes a String to a certain file. Deletes the file if it already exists.
   *
   * @param filepath eg. /tmp/foo.sl
   * @param content the content of the file
   */
  def writeSlToFile( filepath: String, content: String ): Unit = {
    import scalax.file.Path

    val file = Path.fromString( filepath )

    file.createFile( createParents = true, failIfExists = false )

    file.write( content )
  }

}