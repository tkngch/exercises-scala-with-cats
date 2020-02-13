package tkngch.scalawithcats.exercise_1_4

/**
  * 1.4.6 Exercise: Cat Show
  *
  * Re-implement the Cat application from the previous section using Show instead of Printable.
  */
import cats._
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

object CatSyntax {
  // Show.show is a factory method.
  implicit val catShow: Show[Cat] = Show.show { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}

object Main extends App {
  import CatSyntax._
  val cat = Cat(name = "Ollie", age = 6, color = "black")
  println(cat.show)
}
