//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.uom
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

import org.openmath.www.cd._

import rounding1._

trait Rounding1 extends ViewScala with rounding1 {
  // UOM start http://oaff.kwarc.info/openmath/uom?Rounding1?ceiling
  def rounding1_ceiling(a: Term) : Term = {
    Numbers.roundWithOp(a, math.ceil)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Rounding1?ceiling

  // UOM start http://oaff.kwarc.info/openmath/uom?Rounding1?floor
  def rounding1_floor(a: Term) : Term = {
    Numbers.roundWithOp(a, math.floor)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Rounding1?floor

  // UOM start http://oaff.kwarc.info/openmath/uom?Rounding1?round
  def rounding1_round(a: Term) : Term = {
    Numbers.roundWithOp(a, Numbers.round)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Rounding1?round

  // UOM start http://oaff.kwarc.info/openmath/uom?Rounding1?trunc
  def rounding1_trunc(a: Term) : Term = {
    Numbers.roundWithOp(a, Numbers.trunc)
  }
  // UOM end http://oaff.kwarc.info/openmath/uom?Rounding1?trunc

  declares(Implementation.A(rounding1.ceiling.path)(rounding1_ceiling _))
  declares(Implementation.A(rounding1.floor.path)(rounding1_floor _))
  declares(Implementation.A(rounding1.round.path)(rounding1_round _))
  declares(Implementation.A(rounding1.trunc.path)(rounding1_trunc _))

}

object Rounding1 extends Rounding1
