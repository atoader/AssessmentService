//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import permut1._

trait Permut1 extends ViewScala with permut1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Permut1?permutation
  def permut1_permutation(xs1: List[Term]) : Term = {
    throw Unimplemented("permut1_permutation")
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Permut1?permutation

  declares(Implementation.S(permut1.permutation.path)(permut1_permutation _))

}

object Permut1 extends Permut1

