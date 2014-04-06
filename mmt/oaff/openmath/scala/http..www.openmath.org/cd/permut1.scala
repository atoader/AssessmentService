//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait permut1 extends TheoryScala {
  def permut1_permutation(xs1: List[Term]): Term

}

object permut1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "permut1"

  object permutation extends ConstantScala {
    val parent = _path
    val name = "permutation"
    def apply(xs1: List[Term]) = OMA(OMID(this.path), xs1)
    def unapply(t: Term): Option[List[Term]] = t match {
      case OMA(OMID(this.path), xs1) => Some(xs1)
      case _ => None
    }
  }


}

