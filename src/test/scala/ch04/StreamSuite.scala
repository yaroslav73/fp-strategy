package ch04

class StreamSuite extends munit.FunSuite {
  test("Stream should be lazily evaluated") {
    val s = Stream.const(1)
    assertEquals(s.head, 1)
    assertEquals(s.tail.head, 1)
    assertEquals(s.tail.tail.head, 1)
  }

  test("Stream should take n elements correctly") {
    val s = Stream.const(1)
    assertEquals(s.take(0), Nil)
    assertEquals(s.take(1), List(1))
    assertEquals(s.take(5), List(1, 1, 1, 1, 1))
  }

  test("Stream should map correctly") {
    val s = Stream.const(1).map(_ + 1)
    assertEquals(s.take(5), List(2, 2, 2, 2, 2))
  }

  test("Stream should unfold correctly") {
    val s = Stream.unfold(0, identity, _ + 1)
    assertEquals(s.take(5), List(0, 1, 2, 3, 4))
  }

  test("Stream should filter correctly") {
    val s = Stream.unfold(0, identity, _ + 1).filter(_ % 2 == 0)
    assertEquals(s.take(5), List(0, 2, 4, 6, 8))
  }

  test("Stream should zip correctly") {
    val s1     = Stream.unfold(0, identity, _ + 1)
    val s2     = Stream.unfold(10, identity, _ + 1)
    val zipped = s1.zip(s2)
    assertEquals(zipped.take(5), List((0, 10), (1, 11), (2, 12), (3, 13), (4, 14)))
  }

  test("Stream should scanLeft correctly") {
    val s  = Stream.unfold(0, identity, _ + 1).scanLeft(0)(_ + _)
    val s1 = Stream.const(1).scanLeft(0)(_ + _)
    assertEquals(s.take(5), List(0, 1, 3, 6, 10))
    assertEquals(s1.take(5), List(1, 2, 3, 4, 5))
  }
}
