//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.test
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait Rounding extends TheoryScala with org.openmath.www.cd.relation1 with org.openmath.www.cd.logic1 with org.openmath.www.cd.rounding1 {
  val Rounding_floorTest = _assert("Rounding_floorTest", _ => relation1_eq(rounding1_floor(OMF(3.4000000953674316)), OMI(3)), _ == logic1_true())

  val Rounding_ceilingTest = _assert("Rounding_ceilingTest", _ => relation1_eq(rounding1_ceiling(OMF(1.2000000476837158)), OMI(2)), _ == logic1_true())

  val Rounding_roundTest = _assert("Rounding_roundTest", _ => rounding1_round(relation1_eq(OMF(3.700000047683716), OMI(4))), _ == logic1_true())

  val Rounding_truncTest = _assert("Rounding_truncTest", _ => rounding1_trunc(relation1_eq(OMF(3.700000047683716), OMI(3))), _ == logic1_true())

}

object Rounding extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "oaff.kwarc.info") / "openmath" / "test")
  val _path = _base ? "Rounding"

  object floorTest extends ConstantScala {
    val parent = _path
    val name = "floorTest"
  }


  object ceilingTest extends ConstantScala {
    val parent = _path
    val name = "ceilingTest"
  }


  object roundTest extends ConstantScala {
    val parent = _path
    val name = "roundTest"
  }


  object truncTest extends ConstantScala {
    val parent = _path
    val name = "truncTest"
  }


}

