package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.UnitTranslator._
import org.json4s._

trait UnitTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "UnitTranslator" + "Test case 1: Value translation tests" ) {

    it( """Should not accept other values then JInt(0)""" ) {
      intercept[IllegalArgumentException] { UnitTranslator.jsToScalaUnit( JInt( 1 ) ) }
    }

    it( """Should accept the value JInt(0)""" ) {
      UnitTranslator.jsToScalaUnit( JInt( 0 ) )
    }

    it( """Should accept the unit value""" ) {
      UnitTranslator.scalaToJsUnit()
    }

  }

}