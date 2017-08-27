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
}

object MVector {
  def AddHelper(a: MVector, b: MVector): MVector = {
    var bLength = b.coordinates.length
    var aLength = a.coordinates.length
    var newCoords = List[Double]()
    var i = 0
    while(i < aLength){
      if(i < bLength) {
        newCoords = newCoords :+ (a.coordinates(i) + b.coordinates(i))
      } else {
        newCoords = newCoords :+ a.coordinates(i)
      }
      i+=1
    }
    while(i < bLength){
      if(i < aLength) {
        newCoords = newCoords :+ (a.coordinates(i) + b.coordinates(i))
      } else {
        newCoords = newCoords :+ b.coordinates(i)
      }
      i+=1
    }
    return new MVector(newCoords)
  }

  def ScalarMultiply(s: Double, a: MVector): MVector = {
    var newCoords = List[Double]()
    for(i <- 0 to a.coordinates.length - 1) {
      newCoords = newCoords :+ a.coordinates(i) * s
    }
    return new MVector(newCoords)
  }

}