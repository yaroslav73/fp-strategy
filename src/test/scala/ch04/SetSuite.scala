package ch04

import ch04.Set.ListSet

class SetSuite extends munit.FunSuite {
  test("Set should contain inserted elements") {
    val s = ListSet.Empty[Int].insert(1).insert(2).insert(3)
    assert(s.contains(1))
    assert(s.contains(2))
    assert(s.contains(3))
  }

  test("Set should not contain non-inserted elements") {
    val s = ListSet.Empty[Int].insert(1).insert(2).insert(3)
    assert(!s.contains(4))
    assert(!s.contains(5))
  }

  test("Union of two sets should contain all elements") {
    val s1 = ListSet.Empty[Int].insert(1).insert(2)
    val s2 = ListSet.Empty[Int].insert(3).insert(4)
    val union = s1.union(s2)
    assert(union.contains(1))
    assert(union.contains(2))
    assert(union.contains(3))
    assert(union.contains(4))
  }

  test("Union of sets with overlapping elements should not duplicate") {
    val s1 = ListSet.Empty[Int].insert(1).insert(2)
    val s2 = ListSet.Empty[Int].insert(2).insert(3)
    val union = s1.union(s2)
    assert(union.contains(1))
    assert(union.contains(2))
    assert(union.contains(3))
  }
  
  test("Evens set should contain even numbers") {
    assert(Set.Evens.contains(2))
    assert(Set.Evens.contains(4))
    assert(Set.Evens.contains(6))
  }
  
  test("Evens set should not contain odd numbers") {
    assert(!Set.Evens.contains(1))
    assert(!Set.Evens.contains(3))
    assert(!Set.Evens.contains(5))
  }
  
  test("IndicatorSet should work correctly") {
    val isEven = Set.IndicatorSet[Int](_ % 2 == 0)
    assert(isEven.contains(2))
    assert(isEven.contains(4))
    assert(!isEven.contains(1))
    assert(!isEven.contains(3))
  }
}
