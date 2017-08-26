package com.lib.lalib

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable._


@RunWith(classOf[JUnitRunner])
class VectorSpec extends Specification {
  val testVector = new MVector(List(1, 2, 3))

  "Vector" should {
    "print the correct string" in {
      testVector.ToString() mustEqual s"Vector: 1.0, 2.0, 3.0"
    }
    "return true when two vectors are equal" in {
      val newVec = new MVector(List(1, 2, 3))
      testVector.Equals(newVec) mustEqual true
    }
    "return false when two vectors aren't equal" in {
      val newVec = new MVector(List(1, 3, 4, 5))
      testVector.Equals(newVec) mustEqual false
    }
  }
}
