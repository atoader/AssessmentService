//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import list2._

trait List2 extends ViewScala with list2 {
  // UOM start http://oaff.kwarc.info/openmath/uom?List2?list_selector
  def list2_list_selector(i: Term, l: Term) : Term = {
    (i, l) match {
      case (OMI(n), list1.list(ls)) =>
        if (n <= ls.length) ls(n.toInt - 1) else throw UOMException("list2.list_selector index out of bounds")
      case _ => list_selector(i, l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?list_selector

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?first
  def list2_first(l: Term) : Term = {
    l match {
      case list1.list(Nil) => throw UOMException("list2.first: empty list")
      case list1.list(h :: _) => h
      case _ => first(l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?first

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?rest
  def list2_rest(l: Term) : Term = {
    l match {
      case list1.list(Nil) => throw UOMException("list2.rest: empty list")
      case list1.list(_ :: t) => list1.list(t)
      case _ => rest(l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?rest

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?append
  def list2_append(l1: Term, l2: Term) : Term = {
    (l1, l2) match {
      case (list1.list(ls1), list1.list(ls2)) => list1.list(ls1 ::: ls2) 
      case _ => append(l1, l2)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?append

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?cons
  def list2_cons(a: Term, l: Term) : Term = {
    l match {
      case list1.list(ls) => list1.list(a :: ls)
      case _ => cons(a, l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?cons

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?nil
  def list2_nil() : Term = {
    list1.list(Nil)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?nil

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?reverse
  def list2_reverse(l: Term) : Term = {
    l match {
      case list1.list(ls) => list1.list(ls.reverse)
      case _ => reverse(l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?reverse

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?size
  def list2_size(l: Term) : Term = {
    l match {
      case list1.list(ls) => OMI(ls.length)
      case _ => size(l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?size

  // UOM start http://oaff.kwarc.info/openmath/uom?List2?in
  def list2_in(a: Term, l: Term) : Term = {
    l match {
      case list1.list(ls) => logic1.or(ls.map(relation1.OMeq(_, a)))
      case _ => in(a, l)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?List2?in

  declares(Implementation.AA(list2.list_selector.path)(list2_list_selector _))
  declares(Implementation.A(list2.first.path)(list2_first _))
  declares(Implementation.A(list2.rest.path)(list2_rest _))
  declares(Implementation.AA(list2.append.path)(list2_append _))
  declares(Implementation.AA(list2.cons.path)(list2_cons _))
  declares(Implementation.constant(list2.nil.path)(list2_nil _))
  declares(Implementation.A(list2.reverse.path)(list2_reverse _))
  declares(Implementation.A(list2.size.path)(list2_size _))
  declares(Implementation.AA(list2.in.path)(list2_in _))

}

object List2 extends List2

