package de.tuberlin.uebb.sl2.slmacro

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslatorTest
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslatorTest

class TranslationTest
    extends FloatTranslatorTest
    with DoubleTranslatorTest
    with ShortTranslatorTest
    with ByteTranslatorTest
    with IntTranslatorTest
    with LongTranslatorTest
    with BooleanTranslatorTest{

}