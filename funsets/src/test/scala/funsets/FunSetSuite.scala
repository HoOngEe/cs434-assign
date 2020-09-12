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


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
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
    val s4 = singletonSet(4)
    val all = union(union(union(s1, s2), s3), s4)
    val bigger_than_2 = filter(all, x => x > 2)
    val odds = filter(all, x => x % 2 == 1)
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
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }

    new TestSets {
      val s = union(s2, s3)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  test("check intersect elements") {
    new TestSets {
      val s = intersect(union(union(s1, s2), s3),
        union(union(s2, s3), s4))
      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(contains(s, 3), "Intersect 3")
      assert(!contains(s, 4), "Intersect 4")
    }
  }

  test("check diff elements") {
    new TestSets {
      val s = diff(union(union(s1, s2), s4), union(s2, s3))
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
      assert(contains(s, 4), "Diff 4")
    }
  }

  test("check filtered elements") {
    new TestSets {
      assert(!contains(bigger_than_2, 1), "Bigger than 2, 1")
      assert(!contains(bigger_than_2, 2), "Bigger than 2, 2")
      assert(contains(bigger_than_2, 3), "Bigger than 2, 3")
      assert(contains(bigger_than_2, 4), "Bigger than 2, 4")

      assert(contains(odds, 1), "Odds, 1")
      assert(!contains(odds, 2), "Odds, 2")
      assert(contains(odds, 3), "Odds, 3")
      assert(!contains(odds, 4), "Odds, 4")
    }
  }

  test("check forall quantifier") {
    new TestSets {
      assert(forall(all, x => x > -1), "NonNegative")
      assert(forall(bigger_than_2, x => x > 2), "Bigger than 2")
      assert(forall(odds, x => x > 0), "Odds are positive")
    }
  }

  test("check exists quantifier") {
    new TestSets {
      assert(exists(all, x => x < 2), "Elements smaller than 2 exist in all")
      assert(exists(bigger_than_2, x => x > 3), "Elements bigger than 3 exist in bigger_than_2")
      assert(exists(odds, x => x % 3 == 0), "3-multiple elements exist in odds")
    }
  }

  test("check map") {
    new TestSets {
      val evens = map(odds, x => x + 1)
      val squares = map(all, x => x * x)
      assert(forall(evens, x => x % 2 == 0), "odd => odd + 1 map produces evens")
      assert(contains(squares, 1), "Squares 1")
      assert(!contains(squares, 2), "Squares 2")
      assert(!contains(squares, 3), "Squares 3")
      assert(contains(squares, 4), "Squares 4")
      assert(!contains(squares, 5), "Squares 5")
      assert(!contains(squares, 6), "Squares 6")
      assert(!contains(squares, 7), "Squares 7")
      assert(!contains(squares, 8), "Squares 8")
      assert(contains(squares, 9), "Squares 9")
      assert(contains(squares, 16), "Squares 16")
    }
  }
}
