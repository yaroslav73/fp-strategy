package ch08

class AdderSuite extends munit.FunSuite {
  test("Adder.add should sum a list of integers") {
    assertEquals(Adder.add(List(1, 2, 3)), 6)
    assertEquals(Adder.add(List(-1, -2, -3)), -6)
    assertEquals(Adder.add(List()), 0)
  }

  test("Adder.addOpt should sum a list of Option[Int]") {
    assertEquals(Adder.addOpt(List(Some(1), Some(2), Some(3))), 6)
    assertEquals(Adder.addOpt(List(Some(-1), Some(-2), Some(-3))), -6)
    assertEquals(Adder.addOpt(List(None, Some(1), None, Some(2))), 3)
    assertEquals(Adder.addOpt(List()), 0)
  }

  test("Adder.addOrders should calculate total price of orders") {
    val orders = List(
      Adder.Order("A", 2, BigDecimal(10.0)),
      Adder.Order("B", 3, BigDecimal(20.0)),
      Adder.Order("C", 1, BigDecimal(5.0))
    )
    assertEquals(Adder.addOrders(orders), BigDecimal(2 * 10.0 + 3 * 20.0 + 1 * 5.0))

    val emptyOrders = List.empty[Adder.Order]
    assertEquals(Adder.addOrders(emptyOrders), BigDecimal(0))
  }
}
