package tkngch.scalawithcats.exercise_1_3

/**
  * 1.3 Exercise: Printable Library
  *
  * [Link](https://books.underscore.io/scala-with-cats/scala-with-cats.html#exercise-printable-library)
  *
  * Ex.1: Type class instances
  * 1. Define `Printable[A]` with a method `format`. `format` should return a `String`.
  * 2. Create `PrintableInstances` with instances of `Printable` for `String` and `Int`.
  * 3. Define `Printable` with two methods: `format` and `print`.
  *
  * Ex.2: Interface objects
  * 1. Define a case class called `Cat`.
  * 2. Create an implementation of `Printable` for `Cat`.
  * 3. Create a `Cat` and print it.
  *
  * Ex.3: Extension methods
  * 1. Create `PrintableSyntax`.
  * 2. Define `implicit class PrintableOps[A]`.
  * 3. Define the following methods: `format` and `print`.
  * 4. Create a `Cat` and print it.
  */
final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(p.format(value))
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(value: String) = value
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(value: Int) = value.toString
  }

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    def format(value: Cat) = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age year-old $color cat."
    }
  }

}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object Main extends App {
  import PrintableInstances._
  import PrintableSyntax._

  Printable.print("Hello")
  "Hello".print
  Printable.print(100)
  100.print

  val cat = Cat(name = "Ollie", age = 6, color = "black")
  cat.print
  Printable.print(cat)
}
