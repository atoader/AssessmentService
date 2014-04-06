//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import integer1._

trait Integer1 extends ViewScala with integer1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Integer1?factorial
  def integer1_factorial(a: Term) : Term = {
    def fact(n: BigInt) : BigInt =
        if (n == 0 || n == 1) return 1 else return n * fact(n - 1) 
      
      a match {
        case OMI(n) => OMI(fact(n))
        case _ => a
      }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Integer1?factorial

  // UOM start http://oaff.kwarc.info/openmath/uom?Integer1?factorof
  def integer1_factorof(a: Term, b: Term) : Term = {
    (a,b) match {
      case (OMI(m), OMI(n)) => if (n % m == 0) logic1.`true` else logic1.`false`
      case _ => a
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Integer1?factorof

  // UOM start http://oaff.kwarc.info/openmath/uom?Integer1?quotient
  def integer1_quotient(a: Term, b: Term) : Term = {
    (a,b) match {
      case (OMI(m), OMI(n)) => OMI(m / n)
      case _ => a
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Integer1?quotient

  // UOM start http://oaff.kwarc.info/openmath/uom?Integer1?remainder
  def integer1_remainder(a: Term, b: Term) : Term = {
    (a,b) match {
      case (OMI(m), OMI(n)) => OMI(m % n)
      case _ => a
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Integer1?remainder

  declares(Implementation.A(integer1.factorial.path)(integer1_factorial _))
  declares(Implementation.AA(integer1.factorof.path)(integer1_factorof _))
  declares(Implementation.AA(integer1.quotient.path)(integer1_quotient _))
  declares(Implementation.AA(integer1.remainder.path)(integer1_remainder _))

}

object Integer1 extends Integer1
