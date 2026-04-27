package ch08

object Adder:
  def add[T](xs: List[T])(using m: Monoid[T]): T =
    xs.foldLeft(m.empty)(m.combine)

  final case class Order(quantity: Int, price: BigDecimal)
  object Order:
    given orderMonoid: Monoid[Order] with
      def empty: Order = Order(0, BigDecimal(0))
      def combine(x: Order, y: Order): Order =
        Order(quantity = x.quantity + y.quantity, price = x.price + y.price)
