package ch15.v1

import cats.syntax.all.*
import scala.io.StdIn
import scala.util.Try

type Validation[A] = A => Either[String, A]

// The validation rule that always succeeds
def succeed[A](value: A): Either[String, A] = Right(value)

trait Controls[UI[_]] {
  def textInput(label: String, placeholder: String, validation: Validation[String] = succeed): UI[String]
  def choice[A](label: String, options: Seq[(String, A)]): UI[A]
}

object Controls {
  def apply[UI[_]](using controls: Controls[UI]): Controls[UI] = controls
}

trait Layout[UI[_]] {
  def and[A, B](first: UI[A], second: UI[B]): UI[(A, B)]
}

object Layout {
  def apply[UI[_]](using layout: Layout[UI]): Layout[UI] = layout
}

type Program[A] = () => A

object SimpleUI extends Controls[Program], Layout[Program] {
  def textInput(label: String, placeholder: String, validation: Validation[String]): Program[String] =
    new Program[String] {
      def apply(): String =
        def loop(): String = {
          println(s"$label (e.g. $placeholder):")
          val input = StdIn.readLine

          validation(input).fold(msg => { println(msg); loop() }, value => value)
        }

        loop()
    }

  def choice[A](label: String, options: Seq[(String, A)]): Program[A] =
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

  def and[A, B](first: Program[A], second: Program[B]): Program[(A, B)] = (first, second).tupled
}

def quiz[UI[_]](controls: Controls[UI], layout: Layout[UI]): UI[(String, Int)] =
  layout.and(
    controls.textInput("What is your name?", "John Doe"),
    controls.choice(
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

@main def runSimpleUI(): Unit =
  val (name, rating) = quiz[Program](SimpleUI, SimpleUI)()
  println(s"Hello, $name!")
  println(s"You gave tagless final a rating of $rating.")


// Refactoring 1
def name[UI[_]: Controls]: UI[String] =
  Controls[UI].textInput("What is your name?", "John Doe")

def rating[UI[_]: Controls]: UI[Int] =
  Controls[UI].choice(
    "Tagless final is the greatest thing ever",
    Seq(
      "Strongly disagree" -> 1,
      "Disagree" -> 2,
      "Neutral" -> 3,
      "Agree" -> 4,
      "Strongly agree" -> 5
    )
  )

def quiz01[UI[_]: Controls: Layout]: UI[(String, Int)] =
  Layout[UI].and(name, rating)