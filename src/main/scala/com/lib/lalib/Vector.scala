package com.lib.lalib

import scala.collection.immutable.List
import scala.util.control.Breaks._

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