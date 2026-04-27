package ch08

given unionSetMonoid[T]: Monoid[Set[T]] with
  def combine(x: Set[T], y: Set[T]): Set[T] = x.union(y)
  def empty: Set[T]                         = Set.empty

given intersectionSetMonoid[T]: Monoid[Set[T]] with
  def combine(x: Set[T], y: Set[T]): Set[T] = x.intersect(y)
  def empty: Set[T]                         = Set.empty

given diffSetMonoid[T]: Monoid[Set[T]] with
  def combine(x: Set[T], y: Set[T]): Set[T] = x.diff(y)
  def empty: Set[T]                         = Set.empty
