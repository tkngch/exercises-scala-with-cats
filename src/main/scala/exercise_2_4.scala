package tkngch.scalawithcats.exercise_2_4

/**
  * 2.4 Exercise: All Set for Monoids
  *
  * 1. Define monoids and semigroups for `Set`.
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

object SetUnionMonoid {
  // setUnion is a method, not a value, so that it can accept the type parameter A.
  implicit def setUnion[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(set1: Set[A], set2: Set[A]) = set1 union set2
    def empty = Set.empty[A]
  }
}

object SetIntersectMonoid {
  implicit def setIntersect[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(set1: Set[A], set2: Set[A]) = set1 intersect set2
    def empty = Set.empty[A]
  }
}

object Main extends App {
  import SetUnionMonoid._
  val monoid = Monoid[Set[Int]]
  println(monoid.combine(Set(1, 2), Set(2, 3)))
}
