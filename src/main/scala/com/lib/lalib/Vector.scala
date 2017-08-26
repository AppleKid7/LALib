package com.lib.lalib

import scala.collection.immutable.List

class MVector(val coordinates: List[Double]) {
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
}
