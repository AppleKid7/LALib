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
    "addition should return the right sum of vectors for vectors of equal dimensions" in {
      val newVec = new MVector(List(3, 1, 2))
      val expected = new MVector(List(4, 3, 5))
      val result = testVector.Add(newVec)
      result.Equals(expected) mustEqual true
    }
    "addition should return the right sum of vectors when the first vector has fewer dimensions" in {
      val newVec = new MVector(List(3, 1))
      val expected = new MVector(List(4, 3, 3))
      val result = testVector.Add(newVec)
      result.Equals(expected) mustEqual true
    }
    "addition should return the right sum of vectors when the second vector has fewer dimensions" in {
      val newVec = new MVector(List(3, 1, 2, 4))
      val expected = new MVector(List(4, 3, 5, 4))
      val result = testVector.Add(newVec)
      result.Equals(expected) mustEqual true
    }
    "subtraction should return the right sum of vectors for vectors of equal dimensions" in {
      val newVec = new MVector(List(3, 1, 2))
      val expected = new MVector(List(-2, 1, 1))
      val result = testVector.Subtract(newVec)
      result.Equals(expected) mustEqual true
    }
    "subtraction should return the right sum of vectors when the first vector has fewer dimensions" in {
      val newVec = new MVector(List(3, 1))
      val expected = new MVector(List(-2, 1, 3))
      val result = testVector.Subtract(newVec)
      result.Equals(expected) mustEqual true
    }
    "subtraction should return the right sum of vectors when the second vector has fewer dimensions" in {
      val newVec = new MVector(List(3, 1, 2, 4))
      val expected = new MVector(List(-2, 1, 1, -4))
      val result = testVector.Subtract(newVec)
      result.Equals(expected) mustEqual true
    }
  }
}
