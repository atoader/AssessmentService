//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait indNat extends TheoryScala {
  def indNat_zero(): Term

  def indNat_succ(x1: Term): Term

}

object indNat extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "indNat"

  object zero extends ConstantScala {
    val parent = _path
    val name = "zero"
  }


  object succ extends ConstantScala {
    val parent = _path
    val name = "succ"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


}

