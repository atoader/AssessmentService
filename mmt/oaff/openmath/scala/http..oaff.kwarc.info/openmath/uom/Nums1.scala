//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import nums1._

trait Nums1 extends ViewScala with nums1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?NaN
  def nums1_NaN() : Term = {
    OMS(nums1.NaN.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?NaN

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?infinity
  def nums1_infinity() : Term = {
    OMS(nums1.infinity.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?infinity

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?pi
  def nums1_pi() : Term = {
    OMS(nums1.pi.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?pi

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?e
  def nums1_e() : Term = {
    OMS(nums1.e.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?e

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?i
  def nums1_i() : Term = {
    OMS(nums1.i.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?i

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?gamma
  def nums1_gamma() : Term = {
    OMS(nums1.gamma.path)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?gamma

  // UOM start http://oaff.kwarc.info/openmath/uom?Nums1?rational
  def nums1_rational(num: Term, den: Term) : Term = {
    (num, den) match {
		  case (OMI(x), OMI(y)) =>
		    if (y == 0) throw UOMException("nums1.rational: division by zero")
		    if (Numbers.isInt(x.toDouble/y.toDouble)) return OMI(x/y)
		    var x2 = x
		    var y2 = y
		    if (y < 0) {
		      x2 = -x
		      y2 = -y
		    }
		    var gcd = x2.gcd(y2)
		    rational(OMI(x2/gcd), OMI(y2/gcd))
		  case _ => rational(num, den)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Nums1?rational

  declares(Implementation.constant(nums1.NaN.path)(nums1_NaN _))
  declares(Implementation.constant(nums1.infinity.path)(nums1_infinity _))
  declares(Implementation.constant(nums1.pi.path)(nums1_pi _))
  declares(Implementation.constant(nums1.e.path)(nums1_e _))
  declares(Implementation.constant(nums1.i.path)(nums1_i _))
  declares(Implementation.constant(nums1.gamma.path)(nums1_gamma _))
  declares(Implementation.AA(nums1.rational.path)(nums1_rational _))

}

object Nums1 extends Nums1

