package info.kwarc.oaff.openmath.uom

import info.kwarc.mmt.api.objects._
import org.openmath.www.cd.nums1._
import org.openmath.www.cd.complex1._
import org.openmath.www.cd.arith1._
import org.openmath.www.cd.logic1._
import org.openmath.www.cd.relation1._


import scala.math.BigInt.int2bigInt

case class Rational(num: BigInt, den: BigInt) {
   def +(i: BigInt) = copy(num = num + i * den)
   def +(q: Rational) = Rational(num * q.den + q.num * den, den * q.den)
   def *(q: Rational) = Rational(num * q.num, den * q.den)
   
   def toTerm(): Term = if (num % den == 0) OMI(num / den) else rational(OMI(num), OMI(den))
   def toDouble(): Double = num.toDouble/den.toDouble
   def isInt(): Boolean = num % den == 0
}

object Numbers {
  def posDen(q: Term) : Term = {
    q match {
      case rational(OMI(n), OMI(d)) =>
        if (d < 0) return rational(OMI(-n), OMI(-d)) else return q
      case _ => return q
    }
  }
  
  def isInt(x: Double) : Boolean = {
    x == math.floor(x)
  }
  
  def double2Term(x: Double): Term = {
    if (isInt(x)) return OMI(x.toInt) else return OMF(x)
  }
  
  def raise(x: Double, b: Term): Term = {
    b match {
      case OMI(i) => double2Term(math.pow(x, i.toDouble))
      case OMF(f) =>
        var res = math.pow(x, f)
        if (res.isNaN) NaN else double2Term(res)
      case _ => power(double2Term(x), b)
    }
  }
  
  def combineIntFloatParts(op: String, i: BigInt, f: Double) : Term = {
    op match {
      case "+" => f match {
        case 0 => return OMI(i)
        case _ => return OMF(i.toDouble + f)
      }
      case "*" => f match {
        case 1 => return OMI(i)
        case _ => return OMF(i.toDouble * f)
      }
    }
  }
  
  def combineReImParts(re: Term, im: Term) : Term = {
    im match {
      case OMI(i) => if (i == 0) return re else return complex_cartesian(re, im) 
      case OMF(f) => if (f == 0) return re else return complex_cartesian(re, im)
      case _ => return complex_cartesian(re, im)
    }
  }
  
  def cartesianToPolar(re: Term, im: Term) : Term = {
    complex_polar(root(plus(times(re, re), times(im, im)), OMI(2)), atan2(im, re))
  }
  
  /*
   * TRANSCENDENTAL FUNCTIONS
   */
  
  // for example, transc(a, cos(a), math.cos) computes cos(a) if possible
  def transc(a: Term, default: Term, mathfn: Double => Double) : Term = {
    a match {
      case OMI(i) => OMF(mathfn(i.toDouble))
      case OMF(f) => OMF(mathfn(f))
      case _ => default
    }
  }
  
  def atan2(a: Term, b: Term) : Term = {
    (a, b) match {
      case (OMI(x), OMI(y)) => OMF(math.atan2(y.toDouble, x.toDouble))
      case (OMI(x), OMF(y)) => OMF(math.atan2(y, x.toDouble))
      case (OMF(x), OMI(y)) => OMF(math.atan2(y.toDouble, x))
      case (OMF(x), OMF(y)) => OMF(math.atan2(y, x))
      case _ => atan2(a, b)
    }
  }
  
    /*
   * ROUNDING
   */
  
  // round to nearest integer (in case of tie, round to even)
  def round(x: Double): Double = {
    if (isInt(x - 0.5)) // x == *.5, so we have a tie
      if ((x + 0.5).toInt % 2 == 0) x + 0.5 else x - 0.5
    else
      math.round(x)
  }
  
  def trunc(x: Double): Double = {
    if (x >= 0) math.floor(x) else math.ceil(x)
  }

  // applies rounding operation (floor, ceiling, round or truncate) to the term 
  def roundWithOp(t: Term, op: Double => Double): Term = {
    t match {
      case OMI(_) => t
      case OMF(r) => OMI(op(r).toInt)
      case rational(OMI(n), OMI(d)) => OMI(op(n.toDouble/d.toDouble).toInt)
    }
  }
  
  /*
   * COMPARISONS
   */
  
  // checks if t == 0
  def isRealZero(t: Term) : Option[Boolean] = {
    t match {
      case OMI(x) => Some(x == 0)
      case OMF(x) => Some(x == 0)
      case rational(x, _) => isRealZero(x)
      case _ => None
    }
  }

  // checks if a == b, where b = complex_cartesian(re, im)
  def eqRC(a: Term, b: Term, re: Term, im: Term) : Term = {
    isRealZero(im) match {
      case Some(t) => if (t) OMeq(a, re) else `false`
      case _ => OMeq(a,b)
    }
  }
  
  // checks if t < q where t is OMI or OMF and q is Rational
  def ltRQ(t: Term, q: Term) : Boolean = {
    var rational(OMI(n), OMI(d)) = posDen(q)
    t match {
      case OMI(x) => x * d < n
      case OMF(x) => x * d.toDouble < n.toDouble
      case _ => false
    }
  }
  
  // a < b
  def lessThan(a: Term, b: Term) : Option[Boolean] = {
    return (a,b) match {
      case (OMI(x1), OMI(x2)) => Option(x1 < x2)
      case (OMI(x1), OMF(x2)) => Option(x1.toDouble < x2)
      case (OMI(_), rational(_)) => Option(ltRQ(a,b))
      case (OMF(x1), OMI(x2)) => Option(x1 < x2.toDouble)
      case (OMF(x1), OMF(x2)) => Option(x1 < x2)
      case (OMF(x), rational(_)) => Option(ltRQ(a,b))
      case (rational(_), OMI(_)) => Option(ltRQ(b,a))
      case (rational(_), OMF(_)) => Option(ltRQ(b,a))
      case (rational(_), rational(_)) =>
        var rational(OMI(n1), OMI(d1)) = posDen(a)
        var rational(OMI(n2), OMI(d2)) = posDen(b)
        Option(n1 * d2 < n2 * d1)
      case _ => None
    }
  }
  
  // flag is false for min and true for max
  def minMaxList(flag: Boolean, ls: List[Term]) : Term = {
    var res = ls(0)
    for (t <- ls) {
      var cmp = lessThan(t, res)
      (flag, cmp) match {
        case (false, Some(true)) => res = t
        case (true, Some(false)) => res = t
        case _ => 
      }
    }
    return res
  }
  
  
}
