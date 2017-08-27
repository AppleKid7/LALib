package com.lib.lalib

import scala.collection.immutable.List
import scala.math._


case class MalformedVectorsException(message: String) extends Exception(message)

class MVector(private var coordinates: List[Double]) {
  def ToString(): String = {
    var str = ""
    for(num <- coordinates){
      str += num.toString + ", "
    }
    str = str.dropRight(2)
    return s"Vector: ${str}"
  }

  def Equals(v: MVector): Boolean = {
    return coordinates == v.coordinates
  }

  def Add(v: MVector): MVector = {
    return MVector.AddHelper(this, v)
  }

  def Subtract(v: MVector): MVector = {
    val negV = MVector.ScalarMultiply(-1.0, v)
    return Add(negV)
  }

  def ScalarMultiply(s: Double): MVector = {
    return MVector.ScalarMultiply(s, this)
  }

  def Magnitude(): Double = {
    val res = sqrt(coordinates.map(x => x * x).sum)
    return res
  }

  def Normalize(): MVector = {
    val scalar: Double = if(Magnitude() != 0) 1/Magnitude() else 0.0
    return ScalarMultiply(scalar)
  }

  def Dot(v: MVector): Double = {
    val vec1 = coordinates
    val vec2 = v.coordinates
    val res = vec1.zipAll(vec2, 0.0, 0.0).map { case(a, b) => a.asInstanceOf[Double] * b.asInstanceOf[Double] }
    return res.sum
  }

  def Angle(v: MVector, inDegrees: Boolean = false): Double = {
    val norm = Normalize()
    val vNorm = v.Normalize()
    val dotraw = norm.Dot(vNorm)
    val dot = BigDecimal(dotraw).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    if(!inDegrees)
      return acos(dot)
    return acos(dot) * (180.0/math.Pi)
  }

  def isZero(tolerance: Double = 1E-10): Boolean = return Magnitude() < tolerance

  def IsParallel(v: MVector): Boolean = {
    val angle = Angle(v)
    val ret = isZero() || v.isZero() || angle == 0 || angle == math.Pi
    return ret
  }

  def IsOrthogonal(v: MVector, tolerance: Double = 1E-10): Boolean = {
    return abs(Dot(v)) < tolerance
  }

  def ComponentParallelTo(basis: MVector): MVector = {
    val u = basis.Normalize()
    val weight = Dot(u)
    return u.ScalarMultiply(weight)
  }

  def ComponentOrthogonalTo(basis: MVector): MVector = {
    val projection = ComponentParallelTo(basis)
    return Subtract(projection)
  }
}

object MVector {
  def AddHelper(a: MVector, b: MVector): MVector = {
    val vec1 = a.coordinates
    val vec2 = b.coordinates
    val res = vec1.zipAll(vec2, 0.0, 0.0).map { case (a, b) => a.asInstanceOf[Double] + b.asInstanceOf[Double] }
    return new MVector(res)
  }

  def ScalarMultiply(s: Double, a: MVector): MVector = {
    val vec = a.coordinates
    val res = vec.map { case a => a.asInstanceOf[Double] * s}
    return new MVector(res)
  }

}