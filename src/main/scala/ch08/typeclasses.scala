package ch08

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](using m: Monoid[A]): Monoid[A] = m

  given intAddMonoid: Monoid[Int] with
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int                   = 0

  given optionMonoid[T: Monoid]: Monoid[Option[T]] with
    def combine(x: Option[T], y: Option[T]): Option[T] =
      (x, y) match
        case (Some(a), Some(b)) => Some(Monoid[T].combine(a, b))
        case (Some(a), None)    => Some(a)
        case (None, Some(b))    => Some(b)
        case (None, None)       => None
    def empty: Option[T] = None
}
