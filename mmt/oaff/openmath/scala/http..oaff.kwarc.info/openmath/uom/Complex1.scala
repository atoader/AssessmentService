//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import complex1._

trait Complex1 extends ViewScala with complex1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?complex_cartesian
  def complex1_complex_cartesian(re: Term, im: Term) : Term = {
    Numbers.isRealZero(im) match {
      case Some(true) => re
		  case _ => complex_cartesian(re, im)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?complex_cartesian

  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?complex_polar
  def complex1_complex_polar(r: Term, phi: Term) : Term = {
    (Numbers.isRealZero(r), Numbers.isRealZero(phi)) match {
	    case (Some(true), _) => OMI(0)
	    case (_, Some(true)) => r
	    case _ => complex_polar(r, phi) 
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?complex_polar

  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?argument
  def complex1_argument(z: Term) : Term = {
    z match {
	    case complex_cartesian(re, im) => Numbers.atan2(im, re)
	    case complex_polar(r, phi) => phi
	    case _ => argument(z)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?argument

  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?real
  def complex1_real(z: Term) : Term = {
    z match {
	    case complex_cartesian(re, _) => re
	    case complex_polar(r, phi) => arith1.times(r, transc1.cos(phi))
	    case _ => real(z)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?real

  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?imaginary
  def complex1_imaginary(z: Term) : Term = {
    z match {
	    case complex_cartesian(_, im) => im
	    case complex_polar(r, phi) => arith1.times(r, transc1.sin(phi))
	    case _ => imaginary(z)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?imaginary

  // UOM start http://oaff.kwarc.info/openmath/uom?Complex1?conjugate
  def complex1_conjugate(z: Term) : Term = {
    z match {
	    case complex_cartesian(re, im) => complex_cartesian(re, arith1.unary_minus(im))
	    case complex_polar(r, phi) => complex_polar(r, arith1.unary_minus(phi))
	    case _ => argument(z)
	  }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Complex1?conjugate

  declares(Implementation.AA(complex1.complex_cartesian.path)(complex1_complex_cartesian _))
  declares(Implementation.AA(complex1.complex_polar.path)(complex1_complex_polar _))
  declares(Implementation.A(complex1.argument.path)(complex1_argument _))
  declares(Implementation.A(complex1.real.path)(complex1_real _))
  declares(Implementation.A(complex1.imaginary.path)(complex1_imaginary _))
  declares(Implementation.A(complex1.conjugate.path)(complex1_conjugate _))

}

object Complex1 extends Complex1

