package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.EitherTranslator._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator
import org.json4s._

trait EitherTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "EitherTranslator" + "Test case 1: Value translation tests" ) {
    val left = Left( 1 ): Either[Int, Boolean]
    val right = Right( true ): Either[Int, Boolean]

    it( "Should be bijective (Left(1))" ) {
      EitherTranslator.jsToScalaEither( EitherTranslator.scalaToJsEither( left: Either[Int, Boolean], IntTranslator.scalaToJsInt, BooleanTranslator.scalaToJsBool ), IntTranslator.jsToScalaInt, BooleanTranslator.jsToScalaBool ) == ( left: Either[Int, Boolean] )
    }

    it( "Should be bijective (Right(true))" ) {
      EitherTranslator.jsToScalaEither( EitherTranslator.scalaToJsEither( right: Either[Int, Boolean], IntTranslator.scalaToJsInt, BooleanTranslator.scalaToJsBool ), IntTranslator.jsToScalaInt, BooleanTranslator.jsToScalaBool ) == ( right: Either[Int, Boolean] )
    }
  }

}