package de.tuberlin.uebb.sl2.slmacro

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslatorSpec

class TranslationTest
    extends FloatTranslatorSpec
    with DoubleTranslatorSpec
    with ShortTranslatorSpec
    with ByteTranslatorSpec
    with IntTranslatorSpec
    with LongTranslatorSpec
    with BooleanTranslatorSpec{

}