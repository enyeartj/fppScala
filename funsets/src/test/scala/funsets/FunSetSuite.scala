package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
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
    val u12 = union(s1, s2)
    val i12 = intersect(s1, s2)
    val union_all = union(union(s1, s2), s3)
    val i1_all = intersect(union_all, s1)
    val i2_all = intersect(union_all, s2)
    val i3_all = intersect(union_all, s3)
    val i12_1 = intersect(u12, s1)
    val i12_2 = intersect(u12, s2)
    val i12_3 = intersect(u12, s3)
    val d1_all = diff(s1, union_all)
    val dall_1 = diff(union_all, s1)
    val dall_12 = diff(union_all, u12)
    val fall_lt3 = filter(union_all, x => (x < 3))
    val fall_gt1 = filter(union_all, x => (x > 1))
    val ms1_plus_one = map(s1, x => x + 1)
    val mall_plus_five = map(union_all, x => x + 5)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set x contains x`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1 contains 1 Test Failed")
      assert(contains(s2, 2), "Singleton 2 contains 2 Test Failed")
      assert(contains(s3, 3), "Singleton 3 contains 3 Test Failed")
    }
  }

  @Test def `singleton set x  does not contain y`: Unit = {
    new TestSets {
      assert(!contains(s1, 3), "Singleton 1 does not contain 3 Test Failed")
      assert(!contains(s2, 1), "Singleton 2 does not contain 1 Test Failed")
      assert(!contains(s3, 2), "Singleton 3 does not contain 2 Test Failed")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      assert(contains(u12, 1), "Union {1}, {2} contains 1")
      assert(contains(u12, 2), "Union {1}, {2} contains 2")
      assert(!contains(u12, 3), "Union {1}, {2} does not contain 3")
    }
  }

  @Test def `intersection of disjoint sets does not contain anything`: Unit = {
    new TestSets {
      assert(!contains(i12, 1), "Intersection {1}, {2} does not contain 1")
      assert(!contains(i12, 2), "Intersection {1}, {2} does not contain 1")
      assert(!contains(i12, 3), "Intersection {1}, {2} does not contain 1")
    }
  }

  @Test def `intersection of sets contains shared elements`: Unit = {
    new TestSets {
      assert(contains(i1_all, 1), "Intersection {1, 2, 3}, {1} contains 1")
      assert(contains(i2_all, 2), "Intersection {1, 2, 3}, {2} contains 2")
      assert(contains(i3_all, 3), "Intersection {1, 2, 3}, {3} contains 3")
      assert(contains(i12_1, 1), "Intersection {1, 2}, {1} contains 1")
    }
  }

  @Test def `intersection of sets does not contain unshared elements`: Unit = {
    new TestSets {
      assert(!contains(i1_all, 2), "Intersection {1, 2, 3}, {1} does not contain 2")
      assert(!contains(i2_all, 3), "Intersection {1, 2, 3}, {2} does not contain 3")
      assert(!contains(i3_all, 1), "Intersection {1, 2, 3}, {3} does not contain 1")
      assert(!contains(i12_1, 2), "Intersection {1, 2}, {1} does not contain 2")
    }
  }

  @Test def `difference of sets does not contain shared elements`: Unit = {
    new TestSets {
      assert(!contains(d1_all, 1), "Difference {1} - {1, 2, 3} does not contain 1")
      assert(!contains(d1_all, 2), "Difference {1} - {1, 2, 3} does not contain 2")
      assert(!contains(d1_all, 3), "Difference {1} - {1, 2, 3} does not contain 3")
      assert(!contains(dall_1, 1), "Difference {1, 2, 3} - {1} does not contain 1")
      assert(!contains(dall_12, 1), "Difference {1, 2, 3} - {1, 2} does not contain 1")
      assert(!contains(dall_12, 2), "Difference {1, 2, 3} - {1, 2} does not contain 2")
    }
  }

  @Test def `difference of sets contains elements in first set but not in second`: Unit = {
    new TestSets {
      assert(contains(dall_1, 2), "Difference {1, 2, 3} - {1} contains 2")
      assert(contains(dall_1, 3), "Difference {1, 2, 3} - {1} contains 3")
      assert(contains(dall_12, 3), "Difference {1, 2, 3} - {1, 2} contains 3")
    }
  }

  @Test def `filtered set contains elements accepted by predicate`: Unit = {
    new TestSets {
      assert(contains(fall_gt1, 2), "Filter {1, 2, 3} > 1 contains 2")
      assert(contains(fall_gt1, 3), "Filter {1, 2, 3} > 1 contains 3")
      assert(contains(fall_lt3, 1), "Filter {1, 2, 3} < 3 contains 1")
      assert(contains(fall_lt3, 2), "Filter {1, 2, 3} < 3 contains 2")
    }
  }

  @Test def `filtered set does not contain elements unaccepted by predicate`: Unit = {
    new TestSets {
      assert(!contains(fall_gt1, 1), "Filter {1, 2, 3} > 1 does not contain 1")
      assert(!contains(fall_lt3, 3), "Filter {1, 2, 3} < 3 does not contain 3")
    }
  }

  @Test def `forall returns true for sets that match predicate`: Unit = {
    new TestSets {
      assert(forall(union_all, x => (x > 0)), "All elements of {1, 2, 3} > 0")
      assert(forall(union_all, x => (x < 4)), "All elements of {1, 2, 3} < 4")
    }
  }

  @Test def `forall returns false for sets that don't match predicate`: Unit = {
    new TestSets {
      assert(!forall(union_all, x => (x < 0)), "All elements of {1, 2, 3} < 0")
      assert(!forall(union_all, x => (x == 2)), "All elements of {1, 2, 3} == 2")
      assert(!forall(union_all, x => (x < 3)), "All elements of {1, 2, 3} < 3")
      assert(!forall(union_all, x => (x > 1)), "All elements of {1, 2, 3} > 1")
    }
  }

  @Test def `exists returns true for sets with element that match predicate`: Unit = {
    new TestSets {
      assert(exists(union_all, x => (x < 2)), "There exists an element in {1, 2, 3} < 2")
      assert(exists(union_all, x => (x == 2)), "There exists an element in {1, 2, 3} == 2")
      assert(exists(union_all, x => (x > 2)), "There exists an element in {1, 2, 3} > 2")
      assert(exists(union_all, x => (x != 2)), "There exists an element in {1, 2, 3} != 2")
    }
  }

  @Test def `exists returns false for sets without an element that matches predicate`: Unit = {
    new TestSets {
      assert(!exists(union_all, x => (x < 1)), "There does not exist an element in {1, 2, 3} < 1")
      assert(!exists(union_all, x => (x == 5)), "There does not exist an element in {1, 2, 3} == 5")
      assert(!exists(union_all, x => (x > 3)), "There does not exist an element in {1, 2, 3} > 3")
    }
  }

  @Test def `map transforms sets correctly`: Unit = {
    new TestSets {
      assert(contains(ms1_plus_one, 2), "Map {1} + 1 => {2} contains 2")
      assert(!contains(ms1_plus_one, 1), "Map {1} + 1 => {2} does not contain 1")
      assert(forall(mall_plus_five, x => (x > 5)), "Map {1, 2, 3} + 5 => {6, 7, 8} all members > 5")
      assert(forall(mall_plus_five, x => (x < 9)), "Map {1, 2, 3} + 5 => {6, 7, 8} all members < 9")
      assert(contains(mall_plus_five, 6), "Map {1, 2, 3} + 5 => {6, 7, 8} contains 6")
      assert(exists(mall_plus_five, x => (x == 7)), "Map {1, 2, 3} + 5 => {6, 7, 8} there exists member == 7")
      assert(exists(mall_plus_five, x => (x == 8)), "Map {1, 2, 3} + 5 => {6, 7, 8} there exists member == 8")
      assert(!exists(mall_plus_five, x => (x == 5)), "Map {1, 2, 3} + 5 => {6, 7, 8} there does not exist member == 5")
      assert(!exists(mall_plus_five, x => (x == 9)), "Map {1, 2, 3} + 5 => {6, 7, 8} there does not exist member == 9")
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
