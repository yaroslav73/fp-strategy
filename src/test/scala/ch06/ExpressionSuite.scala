package ch06

import Expression.*

class ExpressionSuite extends munit.FunSuite {
  test("Expression: eval Value") {
    assertEquals(Value(42).eval, 42d)
  }

  test("Expression: eval Sum") {
    assertEquals(Sum(Value(1), Value(2)).eval, 3d)
    assertEquals((Value(2) + Value(1)).eval, 3d)
  }

  test("Expression: eval Sub") {
    assertEquals(Sub(Value(5), Value(3)).eval, 2d)
    assertEquals((Value(5) - Value(3)).eval, 2d)
  }

  test("Expression: eval Product") {
    assertEquals(Product(Value(3), Value(4)).eval, 12d)
    assertEquals((Value(3) * Value(4)).eval, 12d)
  }

  test("Expression: eval Division") {
    assertEquals(Division(Value(10), Value(2)).eval, 5d)
    assertEquals((Value(10) / Value(2)).eval, 5d)
  }

  test("Expression: eval nested") {
    val expr01 = Sum(Product(Value(2), Value(3)), Division(Value(10), Value(2)))
    assertEquals(expr01.eval, 11d)

    val expr02 = (Value(2) * Value(3)) + (Value(10) / Value(2))
    assertEquals(expr02.eval, 11d)
  }

  test("Expression: eval with zero division") {
    val expr = Division(Value(10), Value(0))
    assertEquals(expr.eval, Double.PositiveInfinity)
  }

  test("Expression: eval with negative numbers") {
    val expr = Sub(Value(-5), Value(3))
    assertEquals(expr.eval, -8d)
  }

  test("Expression: eval throw StackOverflowError") {
    val expr      = (1 to 10000).foldLeft(Value(1)) { (expr, n) => expr * Value(n) }
    val exception = intercept[StackOverflowError](expr.eval)
    assert(clue(exception.getMessage).contains("StackOverflowError"))
  }
}
