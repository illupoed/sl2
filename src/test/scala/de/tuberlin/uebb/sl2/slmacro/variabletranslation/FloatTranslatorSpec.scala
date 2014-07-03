package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslator._
import org.json4s._

trait FloatTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "FloatTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijective (0.0)" ) {
      val tmp: Float = 0.0f
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Float.MaxValue)" ) {
      val tmp: Float = Float.MaxValue
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Float.MinValue)" ) {
      val tmp: Float = Float.MinValue
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Float.NaN)" ) {
      val tmp: Float = Float.NaN
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Float.PositiveInfinity)" ) {
      val tmp: Float = Float.PositiveInfinity
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Float.NegativeInfinity)" ) {
      val tmp: Float = Float.NegativeInfinity
      FloatTranslator.jsToScalaReal( FloatTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round 1.0)" ) {
      val tmp = JDouble( 1.0 )
      FloatTranslator.scalaToJsReal( FloatTranslator.jsToScalaReal( tmp ) ) == tmp
    }

    it( "Should respect borders of Float (Float.maxValue)" ) {
      val tmp = JDouble( Float.MaxValue * 2.0 )
      intercept[IllegalArgumentException] {
        FloatTranslator.jsToScalaReal( tmp )
      }
    }

    it( "Should respect borders of Float (Float.minValue)" ) {
      val tmp = JDouble( Float.MinValue * 2.0 )
      intercept[IllegalArgumentException] {
        FloatTranslator.jsToScalaReal( tmp )
      }
    }
    
    it( "Should parse also JInt (0)" ) {
      val tmp = JInt( 0 )
      FloatTranslator.jsToScalaReal( tmp ) == 0.0
    }

    val maxDisplayableInteger = BigInt( 2 ).pow( 24 ) - 1

    it( "Should parse also JInt (2 ^ 24 - 1)" ) {
      val tmp = JInt( maxDisplayableInteger )
      FloatTranslator.jsToScalaReal( tmp ) == maxDisplayableInteger.toDouble
    }

    it( "Should parse also JInt (-2 ^ 24 + 1)" ) {
      val tmp = JInt( -maxDisplayableInteger )
      FloatTranslator.jsToScalaReal( tmp ) == -maxDisplayableInteger.toDouble
    }

  }

}