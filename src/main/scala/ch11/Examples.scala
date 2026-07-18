package ch11

import cats.data.OptionT
import cats.syntax.all.*

import ch10.monad.Monad
import ch10.monad.Monad.puree

def compose[M1[_]: Monad, M2[_]: Monad] = {
  type Composed[A] = M1[M2[A]]

  new Monad[Composed] {
    def pure[A](a: A): Composed[A] = a.puree[M2].puree[M1]

    // ffa is m1[m2[a]] * flatMap { fa is m1[a] => ... }
    def flatMap[A, B](ffa: Composed[A])(f: A => Composed[B]): Composed[B] = {
//      TODO: this code will not compile
//      Monad[M1].flatMap(ffa) { fa =>
//        Monad[M2].flatMap(fa) { a => f(a) }
//      }
      ???
    }
  }
}

def optionM[M[_]: Monad] = {
  type Composed[A] = M[Option[A]]

  new Monad[Composed] {
    def pure[A](a: A): Composed[A] = Some(a).puree[M]

    def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
      Monad[M].flatMap(fa)(_.fold[Composed[B]](None.puree[M])(f))

    // The same as above
    def flatMap1[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
      Monad[M].flatMap(fa) {
        case Some(a) => f(a)
        case None    => None.puree[M]
      }
  }
}

@main def transformativeExample01(): Unit = {
  type ListOption[A] = OptionT[List, A]

  val res01: ListOption[Int] = OptionT(List(Option(37)))

  val res02: ListOption[Int] = 37.pure[ListOption]

  val res03 = res01.flatMap { x =>
    res02.map { y =>
      x + y
    }
  }
  
  println(res03.value)
}
