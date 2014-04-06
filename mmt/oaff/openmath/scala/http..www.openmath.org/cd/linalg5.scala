//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait linalg5 extends TheoryScala {
  def linalg5_zero(x1: Term, x2: Term): Term

  def linalg5_identity(x1: Term): Term

  def linalg5_diagonal_matrix(xs1: List[Term]): Term

  def linalg5_scalar(x1: Term, x2: Term): Term

  def linalg5_const(x1: Term, x2: Term): Term

  def linalg5_banded(x1: Term): Term

  def linalg5_symmetric(x1: Term): Term

  def linalg5_skew_symmetric(x1: Term): Term

  def linalg5_Hermitian(x1: Term): Term

  def linalg5_anti_Hermitian(x1: Term): Term

  def linalg5_lower_triangular(x1: Term): Term

  def linalg5_upper_triangular(x1: Term): Term

  def linalg5_tridiagonal(x1: Term): Term

  def linalg5_lower_Hessenberg(x1: Term): Term

  def linalg5_upper_Hessenberg(x1: Term): Term

}

object linalg5 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "linalg5"

  object zero extends ConstantScala {
    val parent = _path
    val name = "zero"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object identity extends ConstantScala {
    val parent = _path
    val name = "identity"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object diagonal_matrix extends ConstantScala {
    val parent = _path
    val name = "diagonal_matrix"
    def apply(xs1: List[Term]) = OMA(OMID(this.path), xs1)
    def unapply(t: Term): Option[List[Term]] = t match {
      case OMA(OMID(this.path), xs1) => Some(xs1)
      case _ => None
    }
  }


  object scalar extends ConstantScala {
    val parent = _path
    val name = "scalar"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object const extends ConstantScala {
    val parent = _path
    val name = "const"
    def apply(x1: Term, x2: Term) = OMA(OMID(this.path), x1 :: x2:: Nil)
    def unapply(t: Term): Option[(Term, Term)] = t match {
      case OMA(OMID(this.path), x1 :: x2:: Nil) => Some((x1, x2))
      case _ => None
    }
  }


  object banded extends ConstantScala {
    val parent = _path
    val name = "banded"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object symmetric extends ConstantScala {
    val parent = _path
    val name = "symmetric"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object skew_symmetric extends ConstantScala {
    val parent = _path
    val name = "skew_symmetric"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object Hermitian extends ConstantScala {
    val parent = _path
    val name = "Hermitian"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object anti_Hermitian extends ConstantScala {
    val parent = _path
    val name = "anti_Hermitian"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object lower_triangular extends ConstantScala {
    val parent = _path
    val name = "lower_triangular"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object upper_triangular extends ConstantScala {
    val parent = _path
    val name = "upper_triangular"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object tridiagonal extends ConstantScala {
    val parent = _path
    val name = "tridiagonal"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object lower_Hessenberg extends ConstantScala {
    val parent = _path
    val name = "lower_Hessenberg"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


  object upper_Hessenberg extends ConstantScala {
    val parent = _path
    val name = "upper_Hessenberg"
    def apply(x1: Term) = OMA(OMID(this.path), x1:: Nil)
    def unapply(t: Term): Option[Term] = t match {
      case OMA(OMID(this.path), x1:: Nil) => Some(x1)
      case _ => None
    }
  }


}
