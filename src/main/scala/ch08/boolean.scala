package ch08

given booleanAndMonoid: Monoid[Boolean] with
  def combine(x: Boolean, y: Boolean): Boolean = x && y
  def empty: Boolean                           = true

given booleanOrMonoid: Monoid[Boolean] with
  def combine(x: Boolean, y: Boolean): Boolean = x || y
  def empty: Boolean                           = false

given booleanNotMonoid: Monoid[Boolean] with
  def combine(x: Boolean, y: Boolean): Boolean = !(x && y)
  def empty: Boolean                           = true

given booleanEitherMonoid: Monoid[Boolean] with
  def combine(a: Boolean, b: Boolean): Boolean = (a && !b) || (!a && b)
  def empty: Boolean                           = false

given booleanXnorMonoid: Monoid[Boolean] with
  def combine(a: Boolean, b: Boolean): Boolean = (!a || b) && (a || !b)
  def empty: Boolean                           = true
