package ch15

object Example1:
  val csiString = "\u001b["

  def printRed(): Unit =
    print(csiString)
    print("31")
    print("m")

  def printReset(): Unit =
    print(csiString)
    print("0")
    print("m")

  @main def go1(): Unit =
    print("Normal text, ")
    printRed()
    print("now red text, ")
    printReset()
    println("and now back to normal.")

object Example2:
  val csiString = "\u001b["
  val redCode   = s"${csiString}31m"
  val resetCode = s"${csiString}0m"

  def printRed(output: String): Unit =
    print(redCode)
    print(output)
    print(resetCode)

  @main def go2(): Unit =
    print("Normal text, ")
    printRed("now red text, ")
    println("and now back to normal.")

object Example3:
  val csiString   = "\u001b["
  val redCode     = s"${csiString}31m"
  val resetCode   = s"${csiString}0m"
  val boldOnCode  = s"${csiString}1m"
  val boldOffCode = s"${csiString}22m"

  def printRed(output: String): Unit =
    print(redCode)
    print(output)
    print(resetCode)

  def printBold(output: String): Unit =
    print(boldOnCode)
    print(output)
    print(boldOffCode)

  def printRedAndBold(output: String): Unit =
    print(redCode)
    print(boldOnCode)
    print(output)
    print(resetCode)

  @main def go3(): Unit =
    print("Normal text, ")
    printRed("now red text, ")
    printBold("and now bold.\n")

object Example4:
  type Program[A] = () => A

  val csiString   = "\u001b["
  val redCode     = s"${csiString}31m"
  val resetCode   = s"${csiString}0m"
  val boldOnCode  = s"${csiString}1m"
  val boldOffCode = s"${csiString}22m"

  def run[A](program: Program[A]): A = program()

  def print(output: String): Program[Unit] =
    () => Console.print(output)

  def printRed[A](output: Program[A]): Program[A] =
    () => {
      run(print(redCode))
      val result = run(output)
      run(print(resetCode))
      result
    }

  def printBold[A](output: Program[A]): Program[A] =
    () => {
      run(print(boldOnCode))
      val result = run(output)
      run(print(boldOffCode))
      result
    }

  @main def go4(): Unit = {
    run(() => {
      run(print("Normal text, "))
      run(printRed(print("now red text, ")))
      run(printBold(print("and now bold ")))
      run(printBold(printRed(print("and now bold and red.\n"))))
    })

    val p = new Program[Unit] {
      def apply(): Unit =
        run(print("Normal text, "))
        run(printRed(print("now red text, ")))
        run(printBold(print("and now bold ")))
        run(printBold(printRed(print("and now bold and red.\n"))))
    }

    p()

    run(
      printBold(() => {
        run(print("This should be bold, "))
        run(printBold(print("as should this ")))
        run(print("and this.\n"))
      })
    )
  }

object Example5:
  import cats.data.State
  import cats.syntax.all.*

  type Program[A] = State[Terminal, A]

  final case class Terminal(bold: Int, color: List[String]) {
    def boldOn: Terminal               = this.copy(bold = bold + 1)
    def boldOff: Terminal              = this.copy(bold = bold - 1)
    def pushColor(c: String): Terminal = this.copy(color = c :: color)
    // Only call this when we know there is at least one color on the stack
    def popColor: Terminal        = this.copy(color = color.tail)
    def peekColor: Option[String] = this.color.headOption
  }

  object Terminal {
    val empty: Terminal = Terminal(0, List.empty)
  }

  object AnsiCodes {
    val csiString: String = "\u001b["

    def csi(arg: String, terminator: String): String =
      s"$csiString$arg$terminator"

    // SGR stands for Select Graphic Rendition.
    // All the codes that change formatting are SGR codes.
    def sgr(arg: String): String = csi(arg, "m")

    val reset: String   = sgr("0")
    val boldOn: String  = sgr("1")
    val boldOff: String = sgr("22")
    val red: String     = sgr("31")
    val blue: String    = sgr("34")
  }

  object Program {
    def print(output: String): Program[Unit] =
      State[Terminal, Unit](terminal => (terminal, Console.print(output)))

    def bold[A](program: Program[A]): Program[A] =
      for {
        _ <- State.modify[Terminal] { terminal =>
          if terminal.bold == 0 then Console.print(AnsiCodes.boldOn)
          terminal.boldOn
        }
        a <- program
        _ <- State.modify[Terminal] { terminal =>
          val newTerminal = terminal.boldOff
          if terminal.bold == 0 then Console.print(AnsiCodes.boldOff)
          newTerminal
        }
      } yield a

    // Helper to construct methods that deal with color
    def withColor[A](code: String)(program: Program[A]): Program[A] =
      for {
        _ <- State.modify[Terminal] { terminal =>
          Console.print(code)
          terminal.pushColor(code)
        }
        a <- program
        _ <- State.modify[Terminal] { terminal =>
          val newTerminal = terminal.popColor
          newTerminal.peekColor match {
            case None    => Console.print(AnsiCodes.reset)
            case Some(c) => Console.print(c)
          }
          newTerminal
        }
      } yield a

    def red[A](program: Program[A]): Program[A] =
      withColor(AnsiCodes.red)(program)

    def blue[A](program: Program[A]): Program[A] =
      withColor(AnsiCodes.blue)(program)

    def green[A](program: Program[A]): Program[A] =
      withColor(AnsiCodes.sgr("32"))(program)

    def run[A](program: Program[A]): A =
      program.runA(Terminal.empty).value
  }

  @main def go5(): Unit = {
    val program =
      Program.blue(
        Program.print("This is blue ") >>
          Program.red(Program.print("and this is red ")) >>
          Program.bold(Program.print("and this is blue and bold "))
      ) >>
        Program.print("and this is back to normal.\n")
    Program.run(program)
    Program.run(Program.green(Program.print("Hello").map(_ => 42)))
  }

object Example6:
  enum Expr {
    case Add(l: Expr, r: Expr)
    case Sub(l: Expr, r: Expr)
    case Mul(l: Expr, r: Expr)
    case Div(l: Expr, r: Expr)

    case Literal(value: Double)
  }

  object EvalInterpreter {
    import Expr.*

    def eval(expr: Expr): Double =
      expr match {
        case Add(l, r)      => eval(l) + eval(r)
        case Sub(l, r)      => eval(l) - eval(r)
        case Mul(l, r)      => eval(l) * eval(r)
        case Div(l, r)      => eval(l) / eval(r)
        case Literal(value) => value
      }
  }

  object PrintInterpreter {
    import Expr.*

    def print(expr: Expr): String =
      expr match {
        case Add(l, r)      => s"(${print(l)} + ${print(r)})"
        case Sub(l, r)      => s"(${print(l)} - ${print(r)})"
        case Mul(l, r)      => s"(${print(l)} * ${print(r)})"
        case Div(l, r)      => s"(${print(l)} / ${print(r)})"
        case Literal(value) => value.toString
      }
  }

  @main def go6(): Unit =
    val onePlusTwo = Expr.Add(Expr.Literal(1), Expr.Literal(2))

    println(EvalInterpreter.eval(onePlusTwo))
    println(PrintInterpreter.print(onePlusTwo))

trait Expr {
  def eval: Double
  def print: String

  def +(that: Expr): Expr = {
    val self = this
    new Expr {
      def eval: Double  = self.eval + that.eval
      def print: String = s"(${self.print} + ${that.print})"
    }
  }

  def -(that: Expr): Expr = {
    val self = this
    new Expr {
      def eval: Double  = self.eval - that.eval
      def print: String = s"(${self.print} - ${that.print})"
    }
  }

  def *(that: Expr): Expr = {
    val self = this
    new Expr {
      def eval: Double  = self.eval * that.eval
      def print: String = s"(${self.print} * ${that.print})"
    }
  }

  def /(that: Expr): Expr = {
    val self = this
    new Expr {
      def eval: Double  = self.eval / that.eval
      def print: String = s"(${self.print} / ${that.print})"
    }
  }

  def sin(expr: Expr): Expr = {
    new Expr {
      def eval: Double  = Math.sin(expr.eval)
      def print: String = s"sin(${expr.print})"
    }
  }
}

object Expr {
  def literal(value: Double): Expr =
    new Expr {
      def eval: Double  = value
      def print: String = value.toString
    }
}

@main def exprExample01(): Unit =
  val onePlusTwo = Expr.literal(1) + Expr.literal(2)
  println(onePlusTwo.eval)
  println(onePlusTwo.print)

// Arithmetic is the program algebra
// Expr is the program type
trait Arithmetic[Expr] {
  def +(l: Expr, r: Expr): Expr
  def -(l: Expr, r: Expr): Expr
  def *(l: Expr, r: Expr): Expr
  def /(l: Expr, r: Expr): Expr

  def literal(value: Double): Expr
}

object DoubleArithmetic extends Arithmetic[Double] {
  def +(l: Double, r: Double): Double = l + r
  def -(l: Double, r: Double): Double = l - r
  def *(l: Double, r: Double): Double = l * r
  def /(l: Double, r: Double): Double = l / r
  def literal(value: Double): Double  = value
}

object PrintArithmetic extends Arithmetic[String] {
  def +(l: String, r: String): String = s"($l + $r)"
  def -(l: String, r: String): String = s"($l - $r)"
  def *(l: String, r: String): String = s"($l * $r)"
  def /(l: String, r: String): String = s"($l / $r)"
  def literal(value: Double): String  = value.toString
}

trait Trigonometry[Expr] {
  def sin(expr: Expr): Expr
}

object DoubleTrigonometry extends Trigonometry[Double] {
  def sin(expr: Double): Double = Math.sin(expr)
}

object PrintTrigonometry extends Trigonometry[String] {
  def sin(expr: String): String = s"sin($expr)"
}

@main def arithmeticExample01(): Unit =
  def onePlusTwo[Expr](arithmetic: Arithmetic[Expr]): Expr =
    arithmetic.+(arithmetic.literal(1.0), arithmetic.literal(2.0))

  def sinOnePlusTwo[Expr](arithmetic: Arithmetic[Expr], trigonometry: Trigonometry[Expr]): Expr =
    trigonometry.sin(onePlusTwo(arithmetic))

  println(onePlusTwo(DoubleArithmetic))
  println(onePlusTwo(PrintArithmetic))
  println(sinOnePlusTwo(PrintArithmetic, PrintTrigonometry))

