package tkngch.scalawithcats.exercise_10

import cats._
import cats.implicits._

object CheckF {
  // Check with functions
  final case class Check[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] =
      func(a)

    def and(that: Check[E, A])(implicit s: Semigroup[E]): Check[E, A] =
      Check { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2))   => (e1 |+| e2).asLeft
          case (Left(e), Right(a))    => e.asLeft
          case (Right(a), Left(e))    => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
      }
  }

  def check(value: Int): Either[List[String], Int] = {
    val a: Check[List[String], Int] =
      Check(v =>
        if (v > 2) v.asRight
        else List("Must be > 2").asLeft
      )

    val b: Check[List[String], Int] =
      Check(v =>
        if (v < -2) v.asRight
        else List("Must be < -2").asLeft
      )

    val aAndB: Check[List[String], Int] = a and b

    aAndB(value)
  }

}
