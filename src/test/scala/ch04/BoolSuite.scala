package ch04

class BoolSuite extends munit.FunSuite {
  import Bool.{ False, True }

  test("AND should work correctly") {
    assertEquals(True.and(True).iff("True")("False"), "True")
    assertEquals(True.and(False).iff("True")("False"), "False")
    assertEquals(False.and(True).iff("True")("False"), "False")
    assertEquals(False.and(False).iff("True")("False"), "False")
  }

  test("OR should work correctly") {
    assertEquals(True.or(True).iff("True")("False"), "True")
    assertEquals(True.or(False).iff("True")("False"), "True")
    assertEquals(False.or(True).iff("True")("False"), "True")
    assertEquals(False.or(False).iff("True")("False"), "False")
  }

  test("NOT should work correctly") {
    assertEquals(True.not.iff("True")("False"), "False")
    assertEquals(False.not.iff("True")("False"), "True")
  }
}
