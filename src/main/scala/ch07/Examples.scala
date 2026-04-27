package ch07

import cats.*
import cats.syntax.all.*

import java.util.Date

@main def showExamples(): Unit =
  val showInt = Show[Int]

  println(showInt.show(123))
  println(123.show)

  // Defining custom Show instances
  given showDate: Show[Date] with
    def show(date: Date): String = s"${date.getTime} ms since epoch"

  println(new Date().show)

  final case class Cat(name: String, age: Int, color: String)
  object Cat:
    given showCat: Show[Cat] =
      Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")

  val fiona = Cat("Fiona", 7, "three-colored")
  println(fiona.show)

@main def eqExamples(): Unit =
  // val res = List(1, 2, 3).map(Option(_)).filter(n => n == 1) // didn't compile

  val eqInt = Eq[Int]

  println(s"eqInt.eqv(123, 123) is ${eqInt.eqv(123, 123)}")
  println(s"eqInt.eqv(123, 124) is ${eqInt.eqv(123, 124)}")
  println(s"123 === 123 is ${123 === 123}")
  println(s"123 =!= 123 is ${123 =!= 123}")

  println(Some(1) == None)
  // println(Some(1) === None) // Error: value === is not a member of Some[Int] - did you mean Some[Int].==?

  // Defining custom Eq instances
  val dateEq: Eq[Date] = Eq.instance((d1, d2) => d1.getTime == d2.getTime)

  extension (d: Date) def ===(other: Date): Boolean = dateEq.eqv(d, other)

  val x = new Date()
  val y = new Date()

  println(s"$x === $x is ${x === x}")
  println(s"$x === $y is ${x === y}")

  final case class Cat(name: String, age: Int, color: String)
  object Cat:
    given eqCat: Eq[Cat] =
      Eq.instance((c1, c2) => c1.name == c2.name && c1.age == c2.age && c1.color == c2.color)

    extension (c1: Cat) def ===(c2: Cat): Boolean = eqCat.eqv(c1, c2)

  val cat1 = Cat("Garfield", 7, "orange and black")
  val cat2 = Cat("Heathcliff", 3, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(s"$cat1 === $cat1 is ${cat1 === cat1}")
  println(s"$cat1 === $cat2 is ${cat1 === cat2}")
  println(s"$optionCat1 === $optionCat2 is ${optionCat1 === optionCat2}")
