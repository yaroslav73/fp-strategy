package ch06

enum Expression {
  case Value(value: Double)
  case Sum(left: Expression, right: Expression)
  case Sub(left: Expression, right: Expression)
  case Product(left: Expression, right: Expression)
  case Division(left: Expression, right: Expression)

  def +(that: Expression): Expression = Sum(this, that)
  def -(that: Expression): Expression = Sub(this, that)
  def *(that: Expression): Expression = Product(this, that)
  def /(that: Expression): Expression = Division(this, that)

  def eval: Double =
    this match {
      case Value(value)          => value
      case Sum(left, right)      => left.eval + right.eval
      case Sub(left, right)      => left.eval - right.eval
      case Product(left, right)  => left.eval * right.eval
      case Division(left, right) => left.eval / right.eval
    }
}
