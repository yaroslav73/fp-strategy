package ch12

import cats.syntax.all.*


@main def parallelListExample01(): Unit = {
  (List(1, 2, 3), List(4, 5, 6)).tupled
  List(1, 2, 3).parProduct(List(4, 5, 6))
}
