package de.tuberlin.uebb.sl2.slmacro.variabletranslation.std

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.SeqTranslator._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslator
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator
import org.json4s._

trait SeqTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "SeqTranslator" + "Test case 1: Value translation tests" ) {
    val empty_seq = Seq()
    val short_seq_int = Seq( 1, 2, 3, 4 )
    val short_seq_boolean = Seq( true, false, true )

    it( "Should be bijective (empty Seq[Int]())" ) {
      SeqTranslator.jsToScalaSeq( SeqTranslator.scalaToJsSeq( empty_seq: Seq[Int], IntTranslator.scalaToJsInt ), IntTranslator.jsToScalaInt ) == ( empty_seq: Seq[Int] )
    }

    it( "Should be bijective (Seq(1,2,3,4))" ) {
      SeqTranslator.jsToScalaSeq( SeqTranslator.scalaToJsSeq( short_seq_int, IntTranslator.scalaToJsInt ), IntTranslator.jsToScalaInt ) == short_seq_int
    }

    it( "Should be bijective (empty Seq[Boolean]())" ) {
      SeqTranslator.jsToScalaSeq( SeqTranslator.scalaToJsSeq( empty_seq: Seq[Boolean], BooleanTranslator.scalaToJsBool ), BooleanTranslator.jsToScalaBool ) == ( empty_seq: Seq[Boolean] )
    }

    it( "Should be bijective (Seq(true, false, true))" ) {
      SeqTranslator.jsToScalaSeq( SeqTranslator.scalaToJsSeq( short_seq_boolean, BooleanTranslator.scalaToJsBool ), BooleanTranslator.jsToScalaBool ) == short_seq_boolean
    }

  }

}