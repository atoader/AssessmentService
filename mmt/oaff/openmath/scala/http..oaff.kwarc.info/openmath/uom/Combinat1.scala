//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import combinat1._

trait Combinat1 extends ViewScala with combinat1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?binomial
  def combinat1_binomial(x1: Term, x2: Term) : Term = {
    throw Unimplemented("combinat1_binomial")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?binomial

  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?multinomial
  def combinat1_multinomial(xs1: List[Term]) : Term = {
    throw Unimplemented("combinat1_multinomial")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?multinomial

  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?Stirling1
  def combinat1_Stirling1(x1: Term, x2: Term) : Term = {
    throw Unimplemented("combinat1_Stirling1")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?Stirling1

  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?Stirling2
  def combinat1_Stirling2(x1: Term, x2: Term) : Term = {
    throw Unimplemented("combinat1_Stirling2")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?Stirling2

  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?Fibonacci
  def combinat1_Fibonacci(x1: Term) : Term = {
    throw Unimplemented("combinat1_Fibonacci")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?Fibonacci

  // UOM start http://oaff.kwarc.info/openmath/uom?Combinat1?Bell
  def combinat1_Bell(x1: Term) : Term = {
    throw Unimplemented("combinat1_Bell")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Combinat1?Bell

  declares(Implementation.AA(combinat1.binomial.path)(combinat1_binomial _))
  declares(Implementation.S(combinat1.multinomial.path)(combinat1_multinomial _))
  declares(Implementation.AA(combinat1.Stirling1.path)(combinat1_Stirling1 _))
  declares(Implementation.AA(combinat1.Stirling2.path)(combinat1_Stirling2 _))
  declares(Implementation.A(combinat1.Fibonacci.path)(combinat1_Fibonacci _))
  declares(Implementation.A(combinat1.Bell.path)(combinat1_Bell _))

}

object Combinat1 extends Combinat1

