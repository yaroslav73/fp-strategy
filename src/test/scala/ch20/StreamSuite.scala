package ch20

class StreamSuite extends munit.FunSuite {
  test("build Stream from Iterator") {
    val s1 = Stream.fromIterator(Iterator(1, 2, 3))
    val s2 = Stream.fromIterator(Iterator(4, 5, 6))
    val s3 = s1.product(s2)

    assertEquals(s3.toSeq, Seq((1, 4), (2, 5), (3, 6)))
    assertEquals(s3.toSeq, Seq.empty)
  }

  test("build Stream from Seq") {
    val s = Stream.fromSeq(Seq(1, 2, 3))

    assertEquals(s.toSeq, Seq(1, 2, 3))
    assertEquals(s.toSeq, Seq(1, 2, 3))
  }

//  TODO: uncomment when Stream will be optimized
//  test("build Stream from big Seq") {
//    val s = Stream.fromSeq(Seq.fill(100_000)(1))
//
//    assertEquals(s.toSeq, Seq.fill(100_000)(1))
//  }
}
