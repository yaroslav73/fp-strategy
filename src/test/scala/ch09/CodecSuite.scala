package ch09

import ch09.Codec.{ decode, encode }

class CodecSuite extends munit.FunSuite {
  given stringCodec: Codec[String] with {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  given intCodec: Codec[Int]         = stringCodec.imap(_.toInt, _.toString)
  given booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
  given doubleCodec: Codec[Double]   = stringCodec.imap(_.toDouble, _.toString)

  test("Codec: String") {
    val original = "Hello, World!"
    val encoded  = original.encode
    val decoded  = encoded.decode[String]

    assertEquals(encoded, "Hello, World!")
    assertEquals(decoded, original)
  }

  test("Codec: Int") {
    val original = 42
    val encoded  = original.encode
    val decoded  = encoded.decode[Int]

    assertEquals(encoded, "42")
    assertEquals(decoded, original)
  }

  test("Codec: Boolean") {
    val originalTrue  = true
    val originalFalse = false

    val encodedTrue  = originalTrue.encode
    val encodedFalse = originalFalse.encode

    val decodedTrue  = encodedTrue.decode[Boolean]
    val decodedFalse = encodedFalse.decode[Boolean]

    assertEquals(encodedTrue, "true")
    assertEquals(encodedFalse, "false")
    assertEquals(decodedTrue, originalTrue)
    assertEquals(decodedFalse, originalFalse)
  }

  test("Codec: Double") {
    val original = 3.14
    val encoded  = original.encode
    val decoded  = encoded.decode[Double]

    assertEquals(encoded, "3.14")
    assertEquals(decoded, original)
  }

  test("Codec: custom type") {
    final case class Box[A](value: A)
    object Box {
      given [A](using c: Codec[A]): Codec[Box[A]] =
        c.imap[Box[A]](value => Box(value), box => box.value)
    }

    val original = Box(42)
    val encoded  = original.encode
    val decoded  = encoded.decode[Box[Int]]

    assertEquals(encoded, "42")
    assertEquals(decoded, original)
  }
}
