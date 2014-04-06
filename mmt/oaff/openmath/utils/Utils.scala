package info.kwarc.oaff.openmath.uom

import info.kwarc.mmt.api.objects._
import org.openmath.www.cd.arith1._
import org.openmath.www.cd.logic1._

object Utils {
  
  // checks if a term is fully known
  // e.g. isFullyKnown[2,3,4] -> true, but isFullyKnown[2,3,P=NP] -> false
  def isFullyKnown(t: Term) : Boolean = {
    t match {
      case OMI(_) | OMF(_) | OMS(_) => true
      case OMA(a, ls) => ls.forall(isFullyKnown)
      case _ => false
    }
  }
  
  def plusWithRest(term: Term, rest: List[Term]): Term = {
    rest match {
      case Nil => term
      case _ => plus(term :: rest)
    }
  }
  
   
  def ombool(b: Boolean) : Term = {
    if (b) `true` else `false` 
  }
}

case class UOMException(msg: String) extends java.lang.Throwable
