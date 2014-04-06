package info.kwarc.oaff.openmath.uom

import info.kwarc.mmt.api.objects._
import org.openmath.www.cd.logic1._
import org.openmath.www.cd.set1._
import org.openmath.www.cd.arith1._

object Collections {
  /*
   * SETS
   */
  
  def collectSets(args: List[Term]): (List[List[Term]], List[Term]) = {
    var sets: List[List[Term]] = Nil
    var restArgs: List[Term] = Nil
    args foreach {
      case emptyset.path =>
      case set(s) => sets ::= s
      case v => restArgs ::= v 
    }
    (sets, restArgs)
  }
  
  def cartesianProduct(xss: List[List[Term]]): List[List[Term]] =
    xss match {
  	  case Nil => List(Nil)
      case h::t => for(xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
   
    
  def nth(i: Int, ls: List[Term]) : Term = {
    if (i > 0 && i <= ls.length) ls(i - 1)
    else throw UOMException(s"nth(${i}, ${ls}): index out of bounds")
  }
  
  
}