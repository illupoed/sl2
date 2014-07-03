package de.tuberlin.uebb.sl2.slmacro.variabletranslation

import org.scalatest.FunSpec
import org.scalatest.matchers._
import de.tuberlin.uebb.sl2.slmacro.variabletranslation.StringTranslator._
import org.json4s._

trait StringTranslatorSpec extends FunSpec with ShouldMatchers {

  describe( "StringTranslator" + "Test case 1: Value translation tests" ) {
    val empty_string = ""
    val very_long_string =
      """this is a very long string
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam eu nunc malesuada, vestibulum augue sodales, aliquam purus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Proin adipiscing ultricies nisi, sit amet pellentesque tortor tristique in. Morbi faucibus sollicitudin elementum. In at gravida dui. Maecenas adipiscing lectus et feugiat congue. Mauris eget dui leo. Pellentesque hendrerit convallis magna non placerat.
        |In vestibulum eget ante nec consectetur. Morbi molestie volutpat nibh ut elementum. Praesent lobortis, libero sed auctor euismod, massa felis pulvinar ipsum, in aliquam ligula risus et ante. Morbi a porttitor nibh, at laoreet lacus. Nunc vitae mauris accumsan, aliquam turpis eu, lacinia risus. Quisque enim ipsum, semper ut dignissim vitae, vehicula sed sem. Cras sit amet eros vitae purus sollicitudin gravida. Vivamus ultricies enim dolor. Suspendisse bibendum enim ac nisl mollis, sed eleifend quam accumsan. Aliquam erat volutpat. Morbi interdum, risus vitae mollis semper, est diam suscipit diam, at egestas felis nisl lacinia ante. Nulla scelerisque neque at purus placerat interdum. Mauris sagittis orci vel risus facilisis, vel accumsan leo egestas. Integer sollicitudin consectetur egestas. Nullam est lectus, facilisis nec ipsum vel, mattis tristique risus. Morbi vitae mauris turpis.
        |Sed iaculis purus quis lacus tristique aliquet. Nam eu varius enim. Nam eget eros quis diam luctus interdum non vitae ipsum. Duis cursus viverra elementum. Vestibulum orci ipsum, rutrum ut vulputate et, consectetur in erat. Nunc rutrum enim vel tortor gravida, sed accumsan augue gravida. Nullam sed lacus in nibh condimentum vestibulum eget vel turpis. Curabitur vitae sem libero. Fusce ut tempus turpis, ornare faucibus nibh. Nullam scelerisque lacinia leo nec tempus. Vivamus cursus dui ut neque blandit consectetur. Sed euismod interdum ante, commodo viverra sapien hendrerit ut. Donec ante mi, sagittis eget varius non, vulputate malesuada velit. Pellentesque quis pulvinar massa.
        |Etiam et congue nisi. Aliquam libero magna, congue vitae egestas nec, fermentum et felis. Maecenas augue turpis, auctor ut ullamcorper suscipit, scelerisque ut lorem. Vivamus non dapibus sem, vel convallis est. Duis laoreet sem blandit rutrum accumsan. Quisque a lacus eleifend, dictum est nec, luctus metus. Sed ac urna in tellus blandit tempus a nec urna.
        |Ut purus magna, imperdiet vitae massa adipiscing, pellentesque blandit sem. Curabitur varius auctor interdum. Morbi nec odio ut felis bibendum pretium. Nulla nunc risus, ornare elementum dictum ut, tristique sed nunc. Aenean molestie dui nec tempus tempus. Sed ut varius dui, id venenatis erat. Pellentesque at sapien eu arcu venenatis pulvinar in et dolor. Integer viverra rutrum dui, a egestas orci varius non.
        |Donec et enim auctor erat ornare sodales vel non quam. Etiam suscipit gravida nibh, id laoreet lectus placerat in. Etiam dignissim at nisi in scelerisque. Cras in lectus sem. Integer dignissim augue sit amet ipsum congue, id aliquam nisl ultricies. Pellentesque feugiat lorem id leo consectetur vehicula. Aenean bibendum id odio eu consectetur. Vestibulum facilisis sit amet ipsum id consectetur. Phasellus id ante erat. Donec varius imperdiet tortor, sed hendrerit sapien adipiscing nec. Vestibulum risus quam, gravida at ultrices eget, dignissim ac elit.
        |Morbi adipiscing vel elit ac imperdiet. Aenean commodo lectus nec mi cursus posuere. Etiam porta in sem vitae vestibulum. Mauris hendrerit lectus at sodales mollis. Cras nec quam massa. Maecenas at fringilla sapien, eu tempor neque. Nulla porttitor est ante, euismod rutrum ligula semper vitae.
        |Nullam ornare, risus vel euismod hendrerit, lorem magna rutrum massa, eget volutpat nisi leo a massa. In vehicula, erat a bibendum lobortis, augue nibh ultrices eros, in volutpat elit nulla ut enim. Aliquam erat sem, congue nec porttitor et, mattis ac turpis. Aenean quis augue et massa commodo sagittis. Interdum et malesuada fames ac ante ipsum primis in faucibus. Maecenas ipsum odio, venenatis eget facilisis ut, ullamcorper quis tellus. Vestibulum consequat dui erat, ac porttitor lectus fringilla a. Fusce adipiscing elit eu quam aliquet pellentesque. Donec turpis sem, pharetra sit amet hendrerit ut, posuere congue ligula. Nam quis nulla vulputate, ultricies orci ac, dictum turpis. Morbi porttitor est sapien, ac lobortis tortor sollicitudin et. Donec ultrices tristique feugiat.
        |""".stripMargin

    it( "Should be bijective (empty String)" ) {
      StringTranslator.jsToScalaString( StringTranslator.scalaToJsString( empty_string ) ) == empty_string
    }

    it( "Should be bijective (very long String)" ) {
      val tmp = """this is a very long string"""
      StringTranslator.jsToScalaString( StringTranslator.scalaToJsString( very_long_string ) ) == very_long_string
    }

    it( "Should be bijective (other way round empty String)" ) {
      val tmp = JString( empty_string )
      StringTranslator.scalaToJsString( StringTranslator.jsToScalaString( tmp ) ) == tmp
    }

    it( "Should be bijective (other way round very long String)" ) {
      val tmp = JString( very_long_string )
      StringTranslator.scalaToJsString( StringTranslator.jsToScalaString( tmp ) ) == tmp
    }

  }

}