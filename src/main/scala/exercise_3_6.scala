package tkngch.scalawithcats.exercise_3_6

/**
  * 3.6.1.1 Exercise: Showing off with Contramap
  * 1. Implement the `contramap` for `Printable`.
  * 2. Define instances of `Printable` for `String` and `Boolean`.
  * 3. Define an instance of `Printable` for `Box` case class, without defining the complete definition (i.e., `new Printable[Box]`).
  *
  * 3.6.2.1 Transformative Thinking with imap
  * 1. Implement `imap` for `Codec`.
  * 2. Create a `Codec` for `Double`.
  * 3. Implement a `Codec` for `Box`.
  */
final case class Box[A](value: A)

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        (func andThen self.format)(value)
    }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String): String = s"string: $value"
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "bool: true" else "bool: false"
  }

  implicit def boxPrintable[B](implicit p: Printable[B]) =
    p.contramap[Box[B]](_.value)
}

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): B = dec(self.decode(value))
    }
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)
}

object CodecInstances {
  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

  implicit val boolCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)
}

import PrintableInstances._
import CodecInstances._

object Main extends App {
  println(Printable.format("hello"))
  println(Printable.format(false))

  println(Printable.format(Box("This is a box.")))
  println(Printable.format(Box(true)))

  println(Codec.encode(Box("this is a box codec.")))
  println(Codec.decode[Double]("12.345"))
}
