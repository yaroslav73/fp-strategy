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

  test("take n elements from Stream") {
    val s = Stream.fromSeq(Seq.fill(100)(47))

    assertEquals(s.take(7).toSeq, Seq.fill(7)(47))
  }

  test("take more elements then Stream contains") {
    val s = Stream.fromSeq(Seq.fill(3)(47))

    assertEquals(s.take(7).toSeq, Seq.fill(3)(47))
  }

  test("merge should combine two Streams") {
    val result = Stream
      .fromSeq(Seq(1, 2, 3))
      .merge(Stream.fromSeq(Seq(4, 5, 6)))
      .toSeq

    assertEquals(result, Seq(1, 2, 3, 4, 5, 6))
  }

//  TODO: uncomment when Stream will be optimized
//  test("build Stream from big Seq") {
//    val s = Stream.fromSeq(Seq.fill(100_000)(1))
//
//    assertEquals(s.toSeq, Seq.fill(100_000)(1))
//  }
}
