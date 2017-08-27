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
    "scalar multiplication should return the right vector" in {
      val scalar = -2
      val expected = new MVector(List(-2, -4, -6))
      val result = testVector.ScalarMultiply(scalar)
      result.Equals(expected) mustEqual true
    }

    "Plus in exercise one should return the appropriate sum" in {
      val vec1 = new MVector(List(8.218, -9.341))
      val vec2 = new MVector(List(-1.129, 2.111))
      val result = vec1.Add(vec2)
      val expected = new MVector(List(7.089, -7.229999999999999))
      result.Equals(expected) mustEqual true
    }

    "Minus in exercise two should return the appropriate subtraction" in {
      val vec1 = new MVector(List(7.119, 8.215))
      val vec2 = new MVector(List(-8.223, 0.878))
      val result = vec1.Subtract(vec2)
      val expected = new MVector(List(15.342, 7.337))
      result.Equals(expected) mustEqual true
    }

    "Scalar multiplication in exercise three should return the appropriate vector" in {
      val scalar = 7.41
      val vec = new MVector(List(1.671, -1.012, -0.318))
      val expected = new MVector(List(12.38211, -7.49892, -2.35638))
      val result = vec.ScalarMultiply(scalar)
      result.Equals(expected) mustEqual true
    }
  }
}
