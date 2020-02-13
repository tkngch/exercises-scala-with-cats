package tkngch.scalawithcats.exercise_10

import cats.Semigroup
import cats.data.Validated

object CheckP {
  // Check with Predicate
  sealed trait Check[E, A, B] {
    def apply(input: A)(implicit semigroup: Semigroup[E]): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, func)
  }

  object Check {
    def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] =
      Pure(predicate)
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C)
      extends Check[E, A, C] {

    def apply(input: A)(implicit semigroup: Semigroup[E]): Validated[E, C] =
      check(input).map(func)
  }

  final case class Pure[E, A](predicate: Predicate[E, A])
      extends Check[E, A, A] {

    def apply(input: A)(implicit semigroup: Semigroup[E]): Validated[E, A] =
      predicate(input)
  }
}
