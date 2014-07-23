package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.Tuple2Translator._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator
import org.json4s._

trait Tuple2TranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "Tuple2Translator" + "Test case 1: Value translation tests" ) {
    val t1 = Tuple2( 1, true ): Tuple2[Int, Boolean]
    val t2 = Tuple2( false, -1 ): Tuple2[Boolean, Int]
    
    println(t1 == t1)
    println(Tuple2Translator.scalaToJsTuple2( t1: Tuple2[Int, Boolean], IntTranslator.scalaToJsInt, BooleanTranslator.scalaToJsBool ))

    it( "Should be bijective (Left(1))" ) {
      Tuple2Translator.jsToScalaTuple2( Tuple2Translator.scalaToJsTuple2( t1: Tuple2[Int, Boolean], IntTranslator.scalaToJsInt, BooleanTranslator.scalaToJsBool ), IntTranslator.jsToScalaInt, BooleanTranslator.jsToScalaBool ) == ( t1: Tuple2[Int, Boolean] )
    }

    it( "Should be bijective (Right(true))" ) {
      Tuple2Translator.jsToScalaTuple2( Tuple2Translator.scalaToJsTuple2( t2: Tuple2[Boolean, Int], BooleanTranslator.scalaToJsBool, IntTranslator.scalaToJsInt ), BooleanTranslator.jsToScalaBool, IntTranslator.jsToScalaInt ) == ( t2: Tuple2[Boolean, Int] )
    }
  }

}