package ch04

trait Set[A] { self =>
  def contains(e: A): Boolean

  def insert(e: A): Set[A] =
    if contains(e) then self
    else
      new Set[A] {
        def contains(e2: A): Boolean =
          e2 == e || self.contains(e2)
      }

  def union(that: Set[A]): Set[A] =
    new Set[A] {
      def contains(e: A): Boolean =
        self.contains(e) || that.contains(e)
    }
}

object Set {
  final case class ListSet[A](elements: List[A]) extends Set[A] {
    def contains(e: A): Boolean =
      elements.contains(e)
  }

  object ListSet {
    def Empty[A]: Set[A] = ListSet(List.empty[A])
  }

  object Evens extends Set[Int] {
    def contains(e: Int): Boolean =
      e % 2 == 0
  }

  final case class IndicatorSet[A](indicator: A => Boolean) extends Set[A] {
    def contains(e: A): Boolean =
      indicator(e)
  }
}
