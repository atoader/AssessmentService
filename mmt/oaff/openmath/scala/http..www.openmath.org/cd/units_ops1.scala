//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait units_ops1 extends TheoryScala {
  def units_ops1_prefix(): Term

}

object units_ops1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "units_ops1"

  object prefix extends ConstantScala {
    val parent = _path
    val name = "prefix"
  }


}
