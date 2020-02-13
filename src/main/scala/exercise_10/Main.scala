package tkngch.scalawithcats.exercise_10

import cats._
import cats.implicits._

object Main extends App {
  val semigroup = Semigroup[List[String]]
  val combination1 = semigroup.combine(List("Badness"), List("More badness"))
  println(combination1)
  val combination2 = List("Oh noes") |+| List("Fail happened")
  println(combination2)

  println(CheckF.check(5))
  println(CheckF.check(0))

  println(CheckE.check(5))
  println(CheckE.check(0))

  println(CheckV.check(5))
  println(CheckV.check(0))

}
