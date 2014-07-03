package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator._
import org.json4s._

trait IntTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "IntTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijective (0.0)" ) {
      val tmp: Int = 0
      IntTranslator.jsToScalaInt( IntTranslator.scalaToJsInt( tmp ) ) == tmp
    }

    it( "Should be bijective (Int.MaxValue)" ) {
      val tmp = Int.MaxValue
      IntTranslator.jsToScalaInt( IntTranslator.scalaToJsInt( tmp ) ) == tmp
    }

    it( "Should be bijective (Int.MinValue)" ) {
      val tmp = Int.MinValue
      IntTranslator.jsToScalaInt( IntTranslator.scalaToJsInt( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round 1.0)" ) {
      val tmp = JInt( 1 )
      IntTranslator.scalaToJsInt( IntTranslator.jsToScalaInt( tmp ) ) == tmp
    }

    it( "Should respect borders of Int (Int.maxValue)" ) {
      val tmp = JInt( Int.MaxValue.toLong + 1 )
      intercept[IllegalArgumentException] {
        IntTranslator.jsToScalaInt( tmp )
      }
    }

    it( "Should respect borders of Int (Int.minValue)" ) {
      val tmp = JInt( Int.MinValue.toLong - 1 )
      intercept[IllegalArgumentException] {
        IntTranslator.jsToScalaInt( tmp )
      }
    }

  }

}