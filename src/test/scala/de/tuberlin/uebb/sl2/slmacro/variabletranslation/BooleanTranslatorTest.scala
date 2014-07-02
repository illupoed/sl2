package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslator._
import org.json4s._

trait BooleanTranslatorTest extends FunSpec with ShouldMatchers {

  describe( "BooleanTranslator" + "Test case 1: Value translation tests" ) {
    it( "Should be bijectiv (true)" ) {
      val tmp: Boolean = true
      BooleanTranslator.jsToScalaBool( BooleanTranslator.scalaToJsBool( tmp ) ) == tmp
    }

    it( "Should be bijectiv (false)" ) {
      val tmp = false
      BooleanTranslator.jsToScalaBool( BooleanTranslator.scalaToJsBool( tmp ) ) == tmp
    }

    it( "Should be bijectiv (other way round true)" ) {
      val tmp = JBool(true)
      BooleanTranslator.scalaToJsBool( BooleanTranslator.jsToScalaBool( tmp ) ) == tmp
    }
    
    it( "Should be bijectiv (other way round false)" ) {
      val tmp = JBool(true)
      BooleanTranslator.scalaToJsBool( BooleanTranslator.jsToScalaBool( tmp ) ) == tmp
    }

  }

}