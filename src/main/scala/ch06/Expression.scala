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

  def eval: Double = {
    type Continuation = Double => Double

    def loop(expr: Expression, c: Continuation): Double =
      expr match {
        case Value(value)          => c(value)
        case Sum(left, right)      => loop(left, l => loop(right, r => c(l + r)))
        case Sub(left, right)      => loop(left, l => loop(right, r => c(l - r)))
        case Product(left, right)  => loop(left, l => loop(right, r => c(l * r)))
        case Division(left, right) => loop(left, l => loop(right, r => c(l / r)))
      }

    loop(this, identity)
  }
}
