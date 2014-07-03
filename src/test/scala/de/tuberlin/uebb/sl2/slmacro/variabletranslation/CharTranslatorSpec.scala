package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.CharTranslator._
import org.json4s._

trait CharTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "CharTranslator" + "Test case 1: Value translation tests" ) {
    val char: Char = 'a'
    val char_not_allowed_to_long = "ab"
    val char_not_allowed_to_short = ""

    it( "Should be bijective ('a')" ) {
      CharTranslator.jsToScalaChar( CharTranslator.scalaToJsChar( char ) ) == char
    }

    it( """Should not accept to long strings ("ab")""" ) {
      intercept[IllegalArgumentException] { CharTranslator.jsToScalaChar( JString( char_not_allowed_to_long ) ) }
    }

    it( """Should not accept to short strings ("")""" ) {
      intercept[IllegalArgumentException] { CharTranslator.jsToScalaChar( JString( char_not_allowed_to_short ) ) }
    }

  }

}