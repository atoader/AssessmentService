//Source file generated by the Universal OpenMath Machine

package info.kwarc.oaff.openmath.test
import info.kwarc.mmt.api._
import objects._
import uom._
import ConstantScala._

trait Linalg extends TheoryScala with org.openmath.www.cd.arith1 with org.openmath.www.cd.relation1 with org.openmath.www.cd.logic1 with org.openmath.www.cd.linalg1 with org.openmath.www.cd.linalg2 with org.openmath.www.cd.linalg4 {
  val Linalg_outerproduct_test = _assert("Linalg_outerproduct_test", _ => relation1_eq(linalg1_outerproduct(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(1), OMI(2), OMI(3)))), linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6))), linalg2_vector(List(OMI(3), OMI(6), OMI(9)))))), _ == logic1_true())

  val Linalg_scalaproduct_test = _assert("Linalg_scalaproduct_test", _ => relation1_eq(linalg1_scalarproduct(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(1), OMI(2), OMI(3)))), OMI(14)), _ == logic1_true())

  val Linalg_vectorproduct_test = _assert("Linalg_vectorproduct_test", _ => relation1_eq(linalg1_vectorproduct(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(3), OMI(4)))), linalg2_vector(List(arith1_unary_minus(OMI(1)), OMI(2), arith1_unary_minus(OMI(1))))), _ == logic1_true())

  val Linalg_vector_selector_test = _assert("Linalg_vector_selector_test", _ => relation1_eq(linalg1_vector_selector(OMI(2), linalg2_vector(List(OMI(1), OMI(2), OMI(3)))), OMI(2)), _ == logic1_true())

  val Linalg_matrix_selector_test = _assert("Linalg_matrix_selector_test", _ => relation1_eq(linalg1_matrix_selector(OMI(2), OMI(3), linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6))), linalg2_vector(List(OMI(3), OMI(6), OMI(9)))))), OMI(6)), _ == logic1_true())

  val Linalg_transpose_test = _assert("Linalg_transpose_test", _ => relation1_eq(linalg1_transpose(linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6)))))), linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(1), OMI(2))), linalg2_vector(List(OMI(2), OMI(2), OMI(4))), linalg2_vector(List(OMI(3), OMI(3), OMI(6)))))), _ == logic1_true())

  val Linalg_size_test = _assert("Linalg_size_test", _ => linalg4_size(relation1_eq(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), OMI(3))), _ == logic1_true())

  val Linalg_rowcount_test = _assert("Linalg_rowcount_test", _ => linalg4_rowcount(relation1_eq(linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6))))), OMI(2))), _ == logic1_true())

  val Linalg_columncount_test = _assert("Linalg_columncount_test", _ => linalg4_columncount(relation1_eq(linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6))), linalg2_vector(List(OMI(3), OMI(6), OMI(9))))), OMI(3))), _ == logic1_true())

  val Linalg_plus_vec_test = _assert("Linalg_plus_vec_test", _ => relation1_eq(linalg2_vector(List(OMI(2), OMI(3), OMI(4))), arith1_plus(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(1), OMI(0), OMI(1))), linalg2_vector(List(OMI(0), OMI(1), OMI(0)))))), _ == logic1_true())

  val Linalg_plus_mat_test = _assert("Linalg_plus_mat_test", _ => relation1_eq(linalg2_matrix(List(linalg2_vector(List(OMI(2), OMI(2), OMI(3))), linalg2_vector(List(OMI(3), OMI(5), OMI(7))))), arith1_plus(List(linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(2), OMI(4), OMI(6))))), linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(0), OMI(0))), linalg2_vector(List(OMI(1), OMI(1), OMI(1)))))))), _ == logic1_true())

  val Linalg_times_mat_test = _assert("Linalg_times_mat_test", _ => relation1_eq(arith1_times(List(linalg2_matrix(List(linalg2_vector(List(OMI(1), OMI(2), OMI(3))), linalg2_vector(List(OMI(4), OMI(5), OMI(6))))), linalg2_matrix(List(linalg2_vector(List(OMI(10), OMI(11))), linalg2_vector(List(OMI(12), OMI(13))), linalg2_vector(List(OMI(14), OMI(15))))))), linalg2_matrix(List(linalg2_vector(List(OMI(76), OMI(82))), linalg2_vector(List(OMI(184), OMI(199)))))), _ == logic1_true())

}

object Linalg extends TheoryScalaAux {
  val _base = DPath(utils.URI("http", "oaff.kwarc.info") / "openmath" / "test")
  val _path = _base ? "Linalg"

  object outerproduct_test extends ConstantScala {
    val parent = _path
    val name = "outerproduct_test"
  }


  object scalaproduct_test extends ConstantScala {
    val parent = _path
    val name = "scalaproduct_test"
  }


  object vectorproduct_test extends ConstantScala {
    val parent = _path
    val name = "vectorproduct_test"
  }


  object vector_selector_test extends ConstantScala {
    val parent = _path
    val name = "vector_selector_test"
  }


  object matrix_selector_test extends ConstantScala {
    val parent = _path
    val name = "matrix_selector_test"
  }


  object transpose_test extends ConstantScala {
    val parent = _path
    val name = "transpose_test"
  }


  object size_test extends ConstantScala {
    val parent = _path
    val name = "size_test"
  }


  object rowcount_test extends ConstantScala {
    val parent = _path
    val name = "rowcount_test"
  }


  object columncount_test extends ConstantScala {
    val parent = _path
    val name = "columncount_test"
  }


  object plus_vec_test extends ConstantScala {
    val parent = _path
    val name = "plus_vec_test"
  }


  object plus_mat_test extends ConstantScala {
    val parent = _path
    val name = "plus_mat_test"
  }


  object times_mat_test extends ConstantScala {
    val parent = _path
    val name = "times_mat_test"
  }


}
