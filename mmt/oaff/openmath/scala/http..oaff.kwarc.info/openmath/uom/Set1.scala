//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import set1._

trait Set1 extends ViewScala with set1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?cartesian_product
  def set1_cartesian_product(args: List[Term]) : Term = {
    var (sets, restArgs) = Collections.collectSets(args)
  	if (restArgs == Nil) {
  	  // we can only compute the product if all arguments are sets
  	  var prod = Collections.cartesianProduct(sets)
  	  set(prod.map(tensor1.tuple(_)))
  	} else {
  		cartesian_product(args)
  	}
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?cartesian_product

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?emptyset
  def set1_emptyset() : Term = {
    set(Nil)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?emptyset

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?map
  def set1_map(f: Term, s: Term) : Term = {
    s match {
      case set(ls) => set(ls map (t => OMA(f, List(t))))
      case interval1.integer_interval(l,r) => (l,r) match {
      	case (OMI(a), OMI(b)) => map(f, set((a to b).toList.map(OMI(_))))
      	case _ => map(f,s)
      }
      case _ => map(f,s)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?map

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?size
  def set1_size(s: Term) : Term = {
    s match {
      case set(ls) => if (Utils.isFullyKnown(s)) OMI(ls.length) else size(s)
      case _ => size(s)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?size

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?suchthat
  def set1_suchthat(s: Term, p: Term) : Term = {
    suchthat(s,p)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?suchthat

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?set
  def set1_set(args: List[Term]) : Term = {
    set(args.distinct.sortBy(_.hashCode))
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?set

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?intersect
  def set1_intersect(args: List[Term]) : Term = {
    var (sets, restArgs) = Collections.collectSets(args)
  	(sets, restArgs) match {
      case (Nil, Nil) => set(Nil)
      case (Nil, _) => intersect(restArgs)
      case _ => 
        var res = set(sets.reduceLeft(_.intersect(_)))
        if (restArgs == Nil) res else intersect(res :: restArgs) 
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?intersect

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?union
  def set1_union(args: List[Term]) : Term = {
    var (sets, restArgs) = Collections.collectSets(args)
  	(sets, restArgs) match {
      case (Nil, Nil) => set(Nil)
      case (Nil, _) => union(restArgs)
      case _ => 
        var res = set(sets.reduceLeft(_.union(_)))
        if (restArgs == Nil) res else union(res :: restArgs) 
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?union

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?setdiff
  def set1_setdiff(s1: Term, s2: Term) : Term = {
    (s1, s2) match {
      case (set(ls1), set(ls2)) => set(ls1.diff(ls2))
      case _ => setdiff(s1,s2)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?setdiff

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?in
  def set1_in(a: Term, s: Term) : Term = {
    s match {
      case set(ls) => logic1.or(ls.map(relation1.OMeq(_, a)))
      case _ => in(a,s)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?in

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?notin
  def set1_notin(a: Term, s: Term) : Term = {
    logic1.not(in(a,s))
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?notin

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?subset
  def set1_subset(s1: Term, s2: Term) : Term = {
    (s1, s2) match {
      case (set(ls1), set(ls2)) => Utils.ombool(ls1.diff(ls2) == Nil)
      case _ => subset(s1,s2)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?subset

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?prsubset
  def set1_prsubset(s1: Term, s2: Term) : Term = {
    s1 match {
    	case set(ls) => if (ls == Nil) logic1.`false` else logic1.and(subset(s1, s2), relation1.neq(s1, s2))
  	  case _ => prsubset(s1, s2)
  	}
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?prsubset

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?notsubset
  def set1_notsubset(s1: Term, s2: Term) : Term = {
    logic1.not(subset(s1, s2))
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?notsubset

  // UOM start http://oaff.kwarc.info/openmath/uom?Set1?notprsubset
  def set1_notprsubset(s1: Term, s2: Term) : Term = {
    logic1.not(prsubset(s1, s2))
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set1?notprsubset

  declares(Implementation.S(set1.cartesian_product.path)(set1_cartesian_product _))
  declares(Implementation.constant(set1.emptyset.path)(set1_emptyset _))
  declares(Implementation.AA(set1.map.path)(set1_map _))
  declares(Implementation.A(set1.size.path)(set1_size _))
  declares(Implementation.AA(set1.suchthat.path)(set1_suchthat _))
  declares(Implementation.S(set1.set.path)(set1_set _))
  declares(Implementation.S(set1.intersect.path)(set1_intersect _))
  declares(Implementation.S(set1.union.path)(set1_union _))
  declares(Implementation.AA(set1.setdiff.path)(set1_setdiff _))
  declares(Implementation.AA(set1.in.path)(set1_in _))
  declares(Implementation.AA(set1.notin.path)(set1_notin _))
  declares(Implementation.AA(set1.subset.path)(set1_subset _))
  declares(Implementation.AA(set1.prsubset.path)(set1_prsubset _))
  declares(Implementation.AA(set1.notsubset.path)(set1_notsubset _))
  declares(Implementation.AA(set1.notprsubset.path)(set1_notprsubset _))

}

object Set1 extends Set1

