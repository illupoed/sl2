package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslator._
import org.json4s._

trait DoubleTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "DoubleTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijective (0.0)" ) {
      val tmp = 0.0
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Double.MaxValue)" ) {
      val tmp = Double.MaxValue
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Double.MinValue)" ) {
      val tmp = Double.MinValue
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Double.NaN)" ) {
      val tmp = Double.NaN
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Double.PositiveInfinity)" ) {
      val tmp = Double.PositiveInfinity
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (Double.NegativeInfinity)" ) {
      val tmp = Double.NegativeInfinity
      DoubleTranslator.jsToScalaReal( DoubleTranslator.scalaToJsReal( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round 1.0)" ) {
      val tmp = JDouble( 1.0 )
      DoubleTranslator.scalaToJsReal( DoubleTranslator.jsToScalaReal( tmp ) ) == tmp
    }

    it( "Should parse also JInt (0)" ) {
      val tmp = JInt( 0 )
      DoubleTranslator.jsToScalaReal( tmp ) == 0.0
    }

    val maxDisplayableInteger = BigInt( 2 ).pow( 53 ) - 1

    it( "Should parse also JInt (2 ^ 53 - 1)" ) {
      val tmp = JInt( maxDisplayableInteger )
      DoubleTranslator.jsToScalaReal( tmp ) == maxDisplayableInteger.toDouble
    }

    it( "Should parse also JInt (-2 ^ 53 + 1)" ) {
      val tmp = JInt( -maxDisplayableInteger )
      DoubleTranslator.jsToScalaReal( tmp ) == -maxDisplayableInteger.toDouble
    }

  }

}