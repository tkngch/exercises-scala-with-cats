package tkngch.scalawithcats.exercise_2_3

/**
  * 2.3 Exercise: The Truth About Monoids
  *
  * 1. Define `combine` and `empty` operations for all the monoids for `Boolean`.
  */
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

object BooleanAndMonoid {
  implicit val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(bool1: Boolean, bool2: Boolean) = bool1 && bool2
    def empty = true
  }
}

object BooleanOrMonoid {
  implicit val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(bool1: Boolean, bool2: Boolean) = bool1 || bool2
    def empty = false
  }
}

object BooleanXorMonoid {
  implicit val booleanXor: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(bool1: Boolean, bool2: Boolean) =
      (!bool1 && bool2) || (bool1 && !bool2)
    def empty = false
  }
}

object BooleanXnorMonoid {
  implicit val booleanXnor: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(bool1: Boolean, bool2: Boolean) =
      (!bool1 || bool2) && (bool1 || !bool2)
    def empty = true
  }
}

object Main extends App {
  import BooleanAndMonoid._
  val monoid = Monoid[Boolean]
  println(monoid.combine(true, true))
  println(monoid.combine(true, false))
}
