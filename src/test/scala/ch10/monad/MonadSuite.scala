package ch10.monad

import munit.FunSuite

class MonadSuite extends FunSuite {
  private val m = Monad[Id]
  test("id monad should return the value") {
    assertEquals(m.pure(42), 42)
  }

  test("id monad should flatMap correctly") {
    val result = m.flatMap(10)(x => x + 5)
    assertEquals(result, 15)
  }

  test("id monad should map correctly") {
    val result = m.map(10)(x => x * 2)
    assertEquals(result, 20)
  }
}
