package ch05

def addAll(a: Int)(using b: Int)(using c: Int): Int =
  a + b + c
  
@main def testAddAll(): Unit =
  given Int = 10
  // given Int = 20 // will be error if uncommented, 
  // because there is already a given Int in scope
  println(addAll(5))
  println(addAll(5)(using 20))
  println(addAll(5)(using 20)(using 25))