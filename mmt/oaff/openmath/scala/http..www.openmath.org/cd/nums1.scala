//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait nums1 extends TheoryScala {
  def nums1_NaN(): Term

  def nums1_infinity(): Term

  def nums1_pi(): Term

  def nums1_e(): Term

  def nums1_i(): Term

  def nums1_gamma(): Term

  def nums1_rational(x1: Term, x2: Term): Term

}

object nums1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "nums1"

  object NaN extends ConstantScala {
    val parent = _path
    val name = "NaN"
  }


  object infinity extends ConstantScala {
    val parent = _path
    val name = "infinity"
  }


  object pi extends ConstantScala {
    val parent = _path
    val name = "pi"
  }


  object e extends ConstantScala {
    val parent = _path
    val name = "e"
  }


  object i extends ConstantScala {
    val parent = _path
    val name = "i"
  }


  object gamma extends ConstantScala {
    val parent = _path
    val name = "gamma"
  }


  object rational extends ConstantScala {
    val parent = _path
    val name = "rational"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


}

