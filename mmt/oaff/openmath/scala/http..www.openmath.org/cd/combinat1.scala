//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait combinat1 extends TheoryScala {
  def combinat1_binomial(x1: Term, x2: Term): Term

  def combinat1_multinomial(xs1: List[Term]): Term

  def combinat1_Stirling1(x1: Term, x2: Term): Term

  def combinat1_Stirling2(x1: Term, x2: Term): Term

  def combinat1_Fibonacci(x1: Term): Term

  def combinat1_Bell(x1: Term): Term

}

object combinat1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "combinat1"

  object binomial extends ConstantScala {
    val parent = _path
    val name = "binomial"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object multinomial extends ConstantScala {
    val parent = _path
    val name = "multinomial"
    def apply(xs1: List[Term]) = OMA(OMID(this.path), xs1)
    def unapply(t: Term): Option[List[Term]] = t match {
      case OMA(OMID(this.path), xs1) => Some(xs1)
      case _ => None
    }
  }


  object Stirling1 extends ConstantScala {
    val parent = _path
    val name = "Stirling1"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object Stirling2 extends ConstantScala {
    val parent = _path
    val name = "Stirling2"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object Fibonacci extends ConstantScala {
    val parent = _path
    val name = "Fibonacci"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object Bell extends ConstantScala {
    val parent = _path
    val name = "Bell"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


}

