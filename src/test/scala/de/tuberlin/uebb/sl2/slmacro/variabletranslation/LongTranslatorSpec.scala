package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslator._
import org.json4s._

trait LongTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "LongTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijective (0.0)" ) {
      val tmp: Long = 0
      LongTranslator.jsToScalaLong( LongTranslator.scalaToJsLong( tmp ) ) == tmp
    }
    
    // because in JS Integers are represented by the Number data type which is in IEEE 754 64-Bit floating point format
    val maxDisplayableInteger = (BigInt( 2 ).pow( 53 ) - 1).toLong

    it( "Should be bijective (2^53 -1)" ) {
      val tmp = maxDisplayableInteger
      LongTranslator.jsToScalaLong( LongTranslator.scalaToJsLong( tmp ) ) == tmp
    }

    it( "Should be bijective (-2^53 +1)" ) {
      val tmp = -maxDisplayableInteger
      LongTranslator.jsToScalaLong( LongTranslator.scalaToJsLong( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round 1.0)" ) {
      val tmp = JInt( 1 )
      LongTranslator.scalaToJsLong( LongTranslator.jsToScalaLong( tmp ) ) == tmp
    }

    it( "Should respect borders of Long in JS form (2^53 -1)" ) {
      val tmp = maxDisplayableInteger + 1
      intercept[IllegalArgumentException] {
        LongTranslator.scalaToJsLong(tmp)
      }
    }

    it( "Should respect borders of Long in JS form (-2^53 +1)" ) {
      val tmp = -maxDisplayableInteger -1
      intercept[IllegalArgumentException] {
          LongTranslator.scalaToJsLong(tmp)
      }
    }

  }

}