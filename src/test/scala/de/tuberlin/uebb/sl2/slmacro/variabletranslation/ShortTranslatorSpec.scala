package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslator._
import org.json4s._

trait ShortTranslatorTest extends FunSpec with ShouldMatchers {

  describe( "ShortTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijectiv (0.0)" ) {
      val tmp: Short = 0
      ShortTranslator.jsToScalaShort( ShortTranslator.scalaToJsShort( tmp ) ) == tmp
    }

    it( "Should be bijectiv (Short.MaxValue)" ) {
      val tmp = Short.MaxValue
      ShortTranslator.jsToScalaShort( ShortTranslator.scalaToJsShort( tmp ) ) == tmp
    }

    it( "Should be bijectiv (Short.MinValue)" ) {
      val tmp = Short.MinValue
      ShortTranslator.jsToScalaShort( ShortTranslator.scalaToJsShort( tmp ) ) == tmp
    }

    it( "Should be bijectiv (other way round 1.0)" ) {
      val tmp = JInt( 1 )
      ShortTranslator.scalaToJsShort( ShortTranslator.jsToScalaShort( tmp ) ) == tmp
    }

    it( "Should respect borders of Short (Short.maxValue)" ) {
      val tmp = JInt( Short.MaxValue + 1 )
      intercept[IllegalArgumentException] {
        ShortTranslator.jsToScalaShort( tmp )
      }
    }

    it( "Should respect borders of Short (Short.minValue)" ) {
      val tmp = JInt( Short.MinValue - 1 )
      intercept[IllegalArgumentException] {
        ShortTranslator.jsToScalaShort( tmp )
      }
    }

  }

}