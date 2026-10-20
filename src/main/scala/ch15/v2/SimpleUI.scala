package ch15.v2

import cats.syntax.all.*
import scala.io.StdIn
import scala.util.Try

type Validation[A] = A => Either[String, A]

// The validation rule that always succeeds
def succeed[A](value: A): Either[String, A] = Right(value)

// Refactoring 2

// Step 1: creating a base type for program algebras
trait Algebra_[UI[_]]

trait Controls_[UI[_]] extends Algebra_[UI] {
  def textInput(label: String, placeholder: String, validation: Validation[String] = succeed): UI[String]
  def choice[A](label: String, options: Seq[(String, A)]): UI[A]
}

trait Layout_[UI[_]] extends Algebra_[UI] {
  def and[A, B](first: UI[A], second: UI[B]): UI[(A, B)]
}

// Step 2: make the program type a type member.
trait Algebra {
  type UI[_]
}

trait Controls extends Algebra {
  def textInput(label: String, placeholder: String, validation: Validation[String] = succeed): UI[String]
  def choice[A](label: String, options: Seq[(String, A)]): UI[A]
}

trait Layout extends Algebra {
  def and[A, B](first: UI[A], second: UI[B]): UI[(A, B)]
}

def quiz02(using alg: Controls & Layout): alg.UI[(String, Int)] =
  alg.and(
    alg.textInput("What is your name?", "John Doe"),
    alg.choice(
      "Tagless final is the greatest thing ever",
      Seq(
        "Strongly disagree" -> 1,
        "Disagree" -> 2,
        "Neutral" -> 3,
        "Agree" -> 4,
        "Strongly agree" -> 5
      )
    )
  )

// Step 3: defining a type for program
// Programs are conceptually functions from an algebra to a program type
// Algebra -> Program
// TODO: Why +A does not compile?
trait Program[-Alg <: Algebra, A] {
  def apply(alg: Alg): alg.UI[A]
}

val quiz01: Program[Controls & Layout, (String, Int)] =
  new Program[Controls & Layout, (String, Int)] {
    def apply(alg: Controls & Layout): alg.UI[(String, Int)] =
      alg.and(
        alg.textInput("What is your name?", "John Doe"),
        alg.choice(
          "Tagless final is the greatest thing ever",
          Seq(
            "Strongly disagree" -> 1,
            "Disagree" -> 2,
            "Neutral" -> 3,
            "Agree" -> 4,
            "Strongly agree" -> 5
          )
        )
      )
  }

// Step 4: defining constructors on companion objects
object Controls {
  def textInput(
    label: String,
    placeholder: String,
    validation: Validation[String] = succeed
  ): Program[Controls, String] =
    alg => alg.textInput(label, placeholder, validation)
  def choice[A](label: String, options: Seq[(String, A)]): Program[Controls, A] =
    alg => alg.choice(label, options)
}

// Step 5: using extension methods for combinators
extension [Alg <: Algebra, A](p: Program[Alg, A]) {
  def and[Alg2 <: Algebra, B](second: Program[Alg2, B]): Program[Alg & Alg2 & Layout, (A, B)] =
    alg => alg.and(p(alg), second(alg))
}

// The interpreter chooses the concrete program type by fixing the `UI` type member.
object SimpleUI extends Controls, Layout {
  // Step 1: pick the program type. Everything else follows from this line.
  type UI[A] = () => A

  // Step 2: implement the constructors. No default for `validation` here:
  // it is inherited from the declaration in `Controls2`.
  def textInput(label: String, placeholder: String, validation: Validation[String]): UI[String] =
    () => {
      def loop(): String = {
        println(s"$label (e.g. $placeholder):")
        val input = StdIn.readLine

        validation(input).fold(msg => { println(msg); loop() }, value => value)
      }

      loop()
    }

  def choice[A](label: String, options: Seq[(String, A)]): UI[A] =
    () => {
      def loop(): A = {
        println(label)
        options.zipWithIndex.foreach { case ((desc, _), idx) => println(s"$idx: $desc") }
        Try(StdIn.readInt).fold(
          _ => { println("Please enter a valid number: "); loop() },
          idx =>
            if idx >= 0 && idx < options.size then options(idx)(1)
            else { println("Please enter a valid number: "); loop() }
        )
      }

      loop()
    }

  // Step 3: implement the combinator.
  def and[A, B](first: UI[A], second: UI[B]): UI[(A, B)] =
    () => (first(), second())
}

val quiz02: Program[Controls & Layout, (String, Int)] =
  Controls
    .textInput("What is your name?", "John Doe")
    .and(
      Controls.choice(
        "Tagless final is the greatest thing ever",
        Seq(
          "Strongly disagree" -> 1,
          "Disagree" -> 2,
          "Neutral" -> 3,
          "Agree" -> 4,
          "Strongly agree" -> 5
        )
      )
    )

@main def runQuiz02(): Unit =
  val (name, rating) = quiz02(SimpleUI)()
  println(s"Hello, $name!")
  println(s"You gave tagless final a rating of $rating.")
