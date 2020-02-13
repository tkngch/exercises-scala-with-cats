package tkngch.scalawithcats.exercise_1_5

/**
  * 1.5.5 Exercise: Equality, Liberty, and Felinity
  *
  * 1. Implement `Eq` for `Cat`.
  * 2. Compare various `Cat`s for equality and inequality.
  */
import cats._
import cats.implicits._

final case class Cat(name: String, age: Int, color: String)

object catEquality {
  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
    }
}

object Main extends App {
  import catEquality._

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  println(cat1 === cat2)
  println(cat1 =!= cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
}
