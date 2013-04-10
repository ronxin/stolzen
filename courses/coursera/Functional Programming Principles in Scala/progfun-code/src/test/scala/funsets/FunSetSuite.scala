package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  test("contains is implemented correctly") {
    def positive(x: Int) = x > 0

    assert(contains(positive, 100))
    assert(!contains(positive, -100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(!contains(s2, 3), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(!contains(s3, 4), "Singleton")
    }
  }

  trait BiggerTestSet {
    def to12(x: Int) = x > 0 && x <= 12
    def from10to20(x: Int) = x > 9 && x <= 20
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }

    new BiggerTestSet {
      val s2 = union(to12, from10to20)
      assert(contains(s2, 1), "Union 1")
      assert(contains(s2, 20), "Union 10")
    }
  }

  test("intersect {1} and {2} is empty") {
    new TestSets {
      val s = intersect(s1, s2)

      assert(!contains(s, 1), "intersect 1")
      assert(!contains(s, 2), "intersect 2")
      assert(!contains(s, 3), "intersect 3")
    }
  }

  test("intersect [1, 12] and [10, 20]") {
    new BiggerTestSet {
      val s2 = intersect(to12, from10to20)
      assert(contains(s2, 10), "contains 10")
      assert(!contains(s2, 1), "doesn't contain 1")
      assert(!contains(s2, 20), "doesn't contain 20")
    }
  }

  test("difference between [1, 12] and [10, 20]") {
    new BiggerTestSet {
      val s2 = diff(to12, from10to20)
      assert(!contains(s2, 10), "doesn't contain 10")
      assert(contains(s2, 1), "contains 1")
      assert(!contains(s2, 20), "doesn't contain 20")
    }
  }

  trait TestSet2 {
    def intervalWithZero(x: Int) = x >= -10 && x <= 10
    val zero = singletonSet(0)
    val interwalWithoutZero = diff(intervalWithZero, zero)
    def nonZeroPredicate(el: Int) = el != 0
    def zeroPredicate(el: Int) = el == 0
  }

  test("forall for [-10, 10] - {0}") {
    new TestSet2 {
      assert(forall(interwalWithoutZero, nonZeroPredicate), "there is no 0")
    }
  }

  test("forall in [-10, 10] and p = x != 0 should yeild negative") {
    new TestSet2 {
      assert(!forall(intervalWithZero, nonZeroPredicate), "there is a zero")
    }
  }

  test("forall edge case: -+bound") {
    val edge = intersect(x => x >= -1001 && x <= -999, x => x >= 999 && x <= 1001)
    assert(forall(edge, el => el >= -bound && el <= bound))
  }

  test("exists for [-10, 10]") {
    new TestSet2 {
      assert(exists(intervalWithZero, nonZeroPredicate), "there is a zero")
    }
  }

  test("exists for [-10, 10] - {0}") {
    new TestSet2 {
      assert(!exists(interwalWithoutZero, nonZeroPredicate), "there is no 0 in the set")
    }
  }

  test("map for [-2, 2] mult by 10") {
    def set(x: Int) = x >= -2 && x <= 2
    val result = map(set, x => x * 10)
    assert(contains(result, -20))
    assert(contains(result, -10))
    assert(contains(result, 0))

    assert(!contains(result, -2))
    assert(!contains(result, -1))
  }

}
