package tkngch.scalawithcats.exercise_4_10

/**
  * 4.10.1 Exercise: Branching out Further with Monads
  * 1. Write a Monad for our Tree data type
  */
import cats.Monad
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Factory {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

}

object MonadMethods {
  implicit val treeMonad = new Monad[Tree] {

    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(left, right) =>
          Branch(flatMap(left)(func), flatMap(right)(func))
        case Leaf(value) => func(value)
      }

    def pure[A](value: A): Tree[A] = Leaf(value)

    def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(func(a)) {
        case Left(value)  => tailRecM(value)(func)
        case Right(value) => Leaf(value)
      }

  }
}

object Main extends App {
  import MonadMethods._

  // with flatMap
  val res1 =
    Factory
      .branch(Factory.leaf(100), Factory.leaf(200))
      .flatMap(x => Factory.branch(Factory.leaf(x - 1), Factory.leaf(x + 1)))
  println(res1)

  // with map
  val res2 = for {
    a <- Factory.branch(Factory.leaf(100), Factory.leaf(200))
    b <- Factory.branch(Factory.leaf(a - 10), Factory.leaf(a + 10))
    c <- Factory.branch(Factory.leaf(b - 1), Factory.leaf(b + 1))
  } yield c
  println(res2)

}
