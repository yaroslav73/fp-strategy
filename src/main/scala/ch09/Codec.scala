package ch09

trait Codec[A] { self => 
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))
    def decode(value: String): B = dec(self.decode(value))
  }
}

object Codec {
  extension [A](value: A) {
    def encode(using c: Codec[A]): String = c.encode(value)
  }

  extension (value: String) {
    def decode[A](using c: Codec[A]): A = c.decode(value)
  }
}
