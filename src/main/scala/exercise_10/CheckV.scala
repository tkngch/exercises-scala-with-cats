package tkngch.scalawithcats.exercise_10

import cats._
import cats.implicits._
import cats.data._

object CheckV {

// Check with Validated
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)

        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)

        case Or(left, right) =>
          left(a) match {
            case Validated.Valid(a) => Validated.Valid(a)
            case Validated.Invalid(err1) =>
              right(a) match {
                case Validated.Valid(b)      => Validated.Valid(b)
                case Validated.Invalid(err2) => Validated.Invalid(err1 |+| err2)
              }
          }
      }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  def check(value: Int): Validated[List[String], Int] = {
    val a: Check[List[String], Int] =
      Pure(v =>
        if (v > 2) Validated.Valid(v)
        else Validated.invalid(List("Must be > 2"))
      )

    val b: Check[List[String], Int] =
      Pure(v =>
        if (v < -2) Validated.Valid(v)
        else Validated.invalid(List("Must be < -2"))
      )

    val aAndB: Check[List[String], Int] =
      a and b

    aAndB(value)
  }
}
