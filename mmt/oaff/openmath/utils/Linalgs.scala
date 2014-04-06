package info.kwarc.oaff.openmath.uom

import info.kwarc.mmt.api.objects._

import org.openmath.www.cd.arith1._
import org.openmath.www.cd.linalg1._
import org.openmath.www.cd.linalg2._

object Linalgs {
  
  def vectorSize(t: Term) : Int = {
    t match {
      case vector(v) => v.length
      case _ => 0
    }
  }

  def termToList(t: Term) : List[Term] = {
    t match {
      case vector(v) => v
      case matrixrow(mr) => mr
      case matrix(m) => m
      case _ => Nil
    }
  }
  
  def stripMatrix(m: Term) : List[List[Term]] = {
    m match {
      case matrix(m) => m.map(termToList)
      case _ => Nil
    }
  }
  
  def mkMatrix(ls: List[List[Term]]) : Term = {
    matrix(ls.map(vector(_)))
  }
  
  def transposeMatrix(m: Term) : Term = {
    mkMatrix(List.transpose(stripMatrix(m)))
  }
  
  def isVecOrMat(t: Term) : Boolean = {
    t match {
      case vector(_) | matrix(_) => true
      case _ => false
    }
  }
  
  def toMatrix(t: Term) : Term = {
    t match {
      case matrix(_) => t
      case v@vector(_) => matrix(v)
    }
  }
  
  def dimMatchMult(a: Term, b: Term) : Boolean = {
    stripMatrix(a)(0).length == termToList(b).length
  }
  
  def binaryMatMult(a: Term, b: Term) : Term = {
    val aRows = termToList(a)
    val bCols = termToList(transposeMatrix(b))
    mkMatrix(aRows.map(aRow => bCols.map(bCol => scalarproduct(aRow, bCol))))
  }
  
  def addVectors(ls: List[List[Term]]) : Term = {
    vector(List.transpose(ls.map(v => v match {
      case List(vector(l)) => l
    })).map(plus(_)))
  } 
}