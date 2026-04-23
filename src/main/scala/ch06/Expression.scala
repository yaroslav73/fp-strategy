package ch06

import scala.annotation.tailrec

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
    type Continuation = Double => Call

    enum Call {
      case Loop(expr: Expression, c: Continuation)
      case Continue(value: Double, c: Continuation)
      case Done(value: Double)
    }

    def loop(expr: Expression, c: Continuation): Call =
      expr match {
        case Value(value)          => Call.Continue(value, c)
        case Sum(left, right)      => Call.Loop(left, l => Call.Loop(right, r => c(l + r)))
        case Sub(left, right)      => Call.Loop(left, l => Call.Loop(right, r => c(l - r)))
        case Product(left, right)  => Call.Loop(left, l => Call.Loop(right, r => c(l * r)))
        case Division(left, right) => Call.Loop(left, l => Call.Loop(right, r => c(l / r)))
      }

    @tailrec
    def trampoline(next: Call): Double =
      next match {
        case Call.Loop(expr, c)      => trampoline(loop(expr, c))
        case Call.Continue(value, c) => trampoline(c(value))
        case Call.Done(value)        => value
      }

    trampoline(loop(this, value => Call.Done(value)))
  }
}
