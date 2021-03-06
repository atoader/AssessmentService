//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait interval1 extends TheoryScala {
  def interval1_integer_interval(x1: Term, x2: Term): Term

  def interval1_interval(x1: Term, x2: Term): Term

  def interval1_interval_cc(x1: Term, x2: Term): Term

  def interval1_interval_co(x1: Term, x2: Term): Term

  def interval1_interval_oc(x1: Term, x2: Term): Term

  def interval1_interval_oo(x1: Term, x2: Term): Term

  def interval1_oriented_interval(x1: Term, x2: Term): Term

}

object interval1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "interval1"

  object integer_interval extends ConstantScala {
    val parent = _path
    val name = "integer_interval"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object interval extends ConstantScala {
    val parent = _path
    val name = "interval"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object interval_cc extends ConstantScala {
    val parent = _path
    val name = "interval_cc"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object interval_co extends ConstantScala {
    val parent = _path
    val name = "interval_co"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object interval_oc extends ConstantScala {
    val parent = _path
    val name = "interval_oc"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object interval_oo extends ConstantScala {
    val parent = _path
    val name = "interval_oo"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object oriented_interval extends ConstantScala {
    val parent = _path
    val name = "oriented_interval"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


}

