package ch08

class AdderSuite extends munit.FunSuite {
  test("Adder.add should sum a list of integers") {
    assertEquals(Adder.add(List(1, 2, 3)), 6)
    assertEquals(Adder.add(List(-1, -2, -3)), -6)
    assertEquals(Adder.add(List.empty[Int]), 0)
  }

  test("Adder.add should sum a list of Option[Int]") {
    assertEquals(Adder.add(List(Option(1), Option(2), Option(3))), Option(6))
    assertEquals(Adder.add(List(Option(-1), Option(-2), Option(-3))), Option(-6))
    assertEquals(Adder.add(List(None, Option(1), None, Option(2))), Option(3))
    assertEquals(Adder.add(List.empty[Option[Int]]), None)
  }

  test("Adder.add should calculate total price of orders") {
    val orders = List(
      Adder.Order(2, BigDecimal(10.0)),
      Adder.Order(3, BigDecimal(20.0)),
      Adder.Order(1, BigDecimal(5.0))
    )
    assertEquals(Adder.add(orders), Adder.Order(6, BigDecimal(35.0)))

    val emptyOrders = List.empty[Adder.Order]
    assertEquals(Adder.add(emptyOrders), Adder.Order(0, 0))
  }
}
