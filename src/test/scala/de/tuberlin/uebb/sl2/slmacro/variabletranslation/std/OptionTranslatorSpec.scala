package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.OptionTranslator._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator
import org.json4s._

trait OptionTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "OptionTranslator" + "Test case 1: Value translation tests" ) {
    val none = None
    val some_int = Some( 1 )
    val some_boolean = Some( true )

    it( "Should be bijective (None[Int])" ) {
      OptionTranslator.jsToScalaOption( OptionTranslator.scalaToJsOption( none: Option[Int], IntTranslator.scalaToJsInt ), IntTranslator.jsToScalaInt ) == ( none: Option[Int] )
    }

    it( "Should be bijective (Some(1))" ) {
      OptionTranslator.jsToScalaOption( OptionTranslator.scalaToJsOption( some_int, IntTranslator.scalaToJsInt ), IntTranslator.jsToScalaInt ) == some_int
    }

    it( "Should be bijective (None[Boolean]())" ) {
      OptionTranslator.jsToScalaOption( OptionTranslator.scalaToJsOption( none: Option[Boolean], BooleanTranslator.scalaToJsBool ), BooleanTranslator.jsToScalaBool ) == ( none: Option[Boolean] )
    }

    it( "Should be bijective (Some(true))" ) {
      OptionTranslator.jsToScalaOption( OptionTranslator.scalaToJsOption( some_boolean, BooleanTranslator.scalaToJsBool ), BooleanTranslator.jsToScalaBool ) == some_boolean
    }

  }

}