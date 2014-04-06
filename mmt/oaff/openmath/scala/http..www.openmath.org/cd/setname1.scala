//Source file generated by the Universal OpenMath Machine

package org.openmath.www.cd
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait setname1 extends TheoryScala {
  def setname1_C(): Term

  def setname1_N(): Term

  def setname1_P(): Term

  def setname1_Q(): Term

  def setname1_R(): Term

  def setname1_Z(): Term

}

object setname1 extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "www.openmath.org") / "cd")
  val _path = _base ? "setname1"

  object C extends ConstantScala {
    val parent = _path
    val name = "C"
  }


  object N extends ConstantScala {
    val parent = _path
    val name = "N"
  }


  object P extends ConstantScala {
    val parent = _path
    val name = "P"
  }


  object Q extends ConstantScala {
    val parent = _path
    val name = "Q"
  }


  object R extends ConstantScala {
    val parent = _path
    val name = "R"
  }


  object Z extends ConstantScala {
    val parent = _path
    val name = "Z"
  }


}

