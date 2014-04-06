//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import set2._

trait Set2 extends ViewScala with set2 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Set2?lift_binary
  def set2_lift_binary(op: Term, s1: Term, s2: Term) : Term = {
    (s1, s2) match {
      case (set1.set(ls1), set1.set(ls2)) => set1.set(Collections.cartesianProduct(List(ls1, ls2)).map(OMA(op, _)))
      case _ => lift_binary(op, s1, s2)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Set2?lift_binary

  declares(Implementation.AAA(set2.lift_binary.path)(set2_lift_binary _))

}

object Set2 extends Set2

