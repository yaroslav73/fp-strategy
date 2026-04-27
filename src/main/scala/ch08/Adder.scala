package ch08

object Adder:
  def add(xs: List[Int]): Int =
    xs.foldLeft(0)(_ + _) // sum

  def addOpt(xs: List[Option[Int]]): Int =
    xs.foldLeft(0) { (acc, x) =>
      x match
        case Some(v) => acc + v
        case None    => acc
    }
    
  final case class Order(id: String, quantity: Int, price: BigDecimal)
    
  def addOrders(orders: List[Order]): BigDecimal =
    orders.foldLeft(BigDecimal(0)) { (acc, order) =>
      acc + (order.quantity * order.price)
    }
