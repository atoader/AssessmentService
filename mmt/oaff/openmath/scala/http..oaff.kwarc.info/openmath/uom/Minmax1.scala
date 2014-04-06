//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import minmax1._

trait Minmax1 extends ViewScala with minmax1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Minmax1?min
  def minmax1_min(a: Term) : Term = {
    a match {
      case set1.set(s) => Numbers.minMaxList(false, s)
      case list1.list(ls) => Numbers.minMaxList(false, ls)
      case _ => OMI(0)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Minmax1?min

  // UOM start http://oaff.kwarc.info/openmath/uom?Minmax1?max
  def minmax1_max(a: Term) : Term = {
    a match {
      case set1.set(s) => Numbers.minMaxList(true, s)
      case list1.list(ls) => Numbers.minMaxList(true, ls)
      case _ => OMI(0)
    }
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Minmax1?max

  declares(Implementation.A(minmax1.min.path)(minmax1_min _))
  declares(Implementation.A(minmax1.max.path)(minmax1_max _))

}

object Minmax1 extends Minmax1
