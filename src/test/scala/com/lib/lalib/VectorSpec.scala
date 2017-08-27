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

    "The Magnitude of the test vector shoud be 3.742" in {
      val expected = 3.7416573867739413
      val result = testVector.Magnitude()
      result mustEqual expected
    }

    "testVector when normalized should be a unit vector" in {
      val expected = new MVector(List(0.2672612419124244, 0.5345224838248488, 0.8017837257372732))
      val result = testVector.Normalize()
      result.Equals(expected) mustEqual true
    }

    "zero vector when normalized should remain the same" in {
      val vec = new MVector(List(0, 0, 0))
      val expected = new MVector(List(0, 0, 0))
      val result = vec.Normalize()
      result.Equals(expected) mustEqual true
    }

    "magnitude exercise 1 must be return the appropriate magnitude" in {
      val vec = new MVector(List(-0.221, 7.437))
      val expected = 7.440282924728065
      val result = vec.Magnitude()
      result mustEqual expected
    }

    "magnitude exercise 2 must be return the appropriate magnitude" in {
      val vec = new MVector(List(8.813, -1.331, -6.247))
      val expected = 10.884187567292289
      val result = vec.Magnitude()
      result mustEqual expected
    }

    "direction exercise 1 must be return the appropriate normalized vector" in {
      val vec = new MVector(List(5.581, -2.136))
      val expected = new MVector(List(0.9339352140866403, -0.35744232526233))
      val result = vec.Normalize()
      result.Equals(expected) mustEqual true
    }

    "direction exercise 1 must be return the appropriate normalized vector" in {
      val vec = new MVector(List(1.996, 3.108, -4.554))
      val expected = new MVector(List(0.3404012959433014, 0.5300437012984873, -0.7766470449528029))
      val result = vec.Normalize()
      result.Equals(expected) mustEqual true
    }

    "Dot product of test vector with itself must yield the correct number" in {
      val vec1 = new MVector(List(1, 2, -1))
      val vec2 = new MVector(List(3, 1, 0))
      val expected = 5
      val result = vec1.Dot(vec2)
      result mustEqual expected
    }

    "Dot product exercise 1 should yield the correct number" in {
      val vec1 = new MVector(List(7.887, 4.138))
      val vec2 = new MVector(List(-8.802, 6.776))
      val expected = -41.382286
      val result = vec1.Dot(vec2)
      result mustEqual expected
    }

    "Dot product exercise 2 should yield the correct number" in {
      val vec1 = new MVector(List(-5.955, -4.904, -1.874))
      val vec2 = new MVector(List(-4.496, -8.755, 7.103))
      val expected = 56.397178000000004
      val result = vec1.Dot(vec2)
      result mustEqual expected
    }
    "Angle exercise 1 should yield the correct number" in {
      val vec1 = new MVector(List(3.183, -7.627))
      val vec2 = new MVector(List(-2.668, 5.319))
      val expected = 3.072
      val result = vec1.Angle(vec2)
      result - expected < 0.001 mustEqual true
    }

    "Angle exercise 2 should yield the correct number" in {
      val vec1 = new MVector(List(7.35, 0.221, 5.188))
      val vec2 = new MVector(List(2.751, 8.259, 3.985))
      val expected = 60.276
      val result = vec1.Angle(vec2, inDegrees = true)
      result - expected < 0.001 mustEqual true
    }

    "Exercise 1 is parallel" in {
      val vec1 = new MVector(List(-7.579, -7.88))
      val vec2 = new MVector(List(22.737, 23.64))
      val result1 = vec1.IsParallel(vec2)
      val result2 = vec1.IsOrthogonal(vec2)
      result1 mustEqual true
      result2 mustEqual false
    }

    "Exercise 2 is neither parallel nor orthogonal" in {
      val vec1 = new MVector(List(-2.029, 9.97, 4.172))
      val vec2 = new MVector(List(-9.231, -6.639, -7.245))
      val result1 = vec1.IsParallel(vec2)
      val result2 = vec1.IsOrthogonal(vec2)
      result1 mustEqual false
      result2 mustEqual false
    }

    "Exercise 3 is orthogonal" in {
      val vec1 = new MVector(List(-2.328, -7.284, -1.214))
      val vec2 = new MVector(List(-1.821, 1.072, -2.94))
      val result1 = vec1.IsParallel(vec2)
      val result2 = vec1.IsOrthogonal(vec2)
      result1 mustEqual false
      result2 mustEqual true
    }

  }
}
