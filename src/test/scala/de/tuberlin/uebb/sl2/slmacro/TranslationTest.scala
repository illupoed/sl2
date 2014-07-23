package de.tuberlin.uebb.sl2.slmacro

import de.tuberlin.uebb.sl2.slmacro.variabletranslation.FloatTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.DoubleTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ShortTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.ByteTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.IntTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.LongTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.BooleanTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.StringTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.CharTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.UnitTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.SeqTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.OptionTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.EitherTranslatorSpec
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.std.Tuple2TranslatorSpec

class TranslationTest
  extends FloatTranslatorSpec
  with DoubleTranslatorSpec
  with ShortTranslatorSpec
  with ByteTranslatorSpec
  with IntTranslatorSpec
  with LongTranslatorSpec
  with BooleanTranslatorSpec
  with StringTranslatorSpec
  with CharTranslatorSpec
  with UnitTranslatorSpec
  with SeqTranslatorSpec
  with OptionTranslatorSpec
  with EitherTranslatorSpec
  with Tuple2TranslatorSpec {}