package info.kwarc.oaff.openmath.uom

import info.kwarc.mmt.api.objects._
import org.openmath.www.cd.arith1._
import org.openmath.www.cd.polyu._

object Polys {
    // add the two list of terms p1, p2
/*  def addPolyTerms(ls1: List[Term], ls2: List[Term]) : List[Term] = {
    val map1 = ls1.map(_ match {
      case term(exp, coeff) => (exp, coeff)
    }).toMap
    
    val map2 = ls2.map(_ match {
      case term(exp, coeff) => (exp, coeff)
    }).toMap
    
    (map1 ++ map2.map {
      case (exp, coeff) => exp -> plus(coeff, map1.getOrElse(exp, OMI(0)))
    }).toList.map(t => term(t._1, t._2))
  }*/
  

  def addPolyTerms(ls1: List[Term], ls2: List[Term]) : List[Term] = {
    (ls1, ls2) match {
      case (Nil, _) => ls2
      case (_, Nil) => ls1
      case ((t1@term(e1, c1)) :: rest1, (t2@term(e2, c2)) :: rest2) =>
        (e1, e2) match {
          case (OMI(n), OMI(m)) =>
            if (n > m) t1 :: addPolyTerms(rest1, ls2)
            else if (n < m) t2 :: addPolyTerms(ls1, rest2)
            else term(e1, plus(c1, c2)) :: addPolyTerms(rest1, rest2)
          case (OMI(n), _) => t2 :: addPolyTerms(ls1, rest2)
          case (_, OMI(m)) => t1 :: addPolyTerms(rest1, ls2)
          case _ => t1 :: t2 :: addPolyTerms(rest1, rest2)
        }
    }
  }
  
  def addPolys(polys: List[Term]) : Term = {
    var terms: List[List[Term]] = Nil
    polys.foreach {
      // TODO take variable into account
      case poly_u_rep(_, ls) => terms ::= ls
      case _ =>
    }
    poly_u_rep(OMI(0), terms.reduceLeft(addPolyTerms))
  }
}