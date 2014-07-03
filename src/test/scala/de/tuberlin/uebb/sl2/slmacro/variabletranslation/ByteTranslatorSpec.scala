package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslator._
import org.json4s._

trait ByteTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "ByteTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijective (0.0)" ) {
      val tmp: Byte = 0
      ByteTranslator.jsToScalaByte( ByteTranslator.scalaToJsByte( tmp ) ) == tmp
    }

    it( "Should be bijective (Byte.MaxValue)" ) {
      val tmp = Byte.MaxValue
      ByteTranslator.jsToScalaByte( ByteTranslator.scalaToJsByte( tmp ) ) == tmp
    }

    it( "Should be bijective (Byte.MinValue)" ) {
      val tmp = Byte.MinValue
      ByteTranslator.jsToScalaByte( ByteTranslator.scalaToJsByte( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round 1.0)" ) {
      val tmp = JInt( 1 )
      ByteTranslator.scalaToJsByte( ByteTranslator.jsToScalaByte( tmp ) ) == tmp
    }

    it( "Should respect borders of Byte (Byte.maxValue)" ) {
      val tmp = JInt( Byte.MaxValue + 1 )
      intercept[IllegalArgumentException] {
        ByteTranslator.jsToScalaByte( tmp )
      }
    }

    it( "Should respect borders of Byte (Byte.minValue)" ) {
      val tmp = JInt( Byte.MinValue - 1 )
      intercept[IllegalArgumentException] {
        ByteTranslator.jsToScalaByte( tmp )
      }
    }

  }

}