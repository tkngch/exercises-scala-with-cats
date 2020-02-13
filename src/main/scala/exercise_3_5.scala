package tkngch.scalawithcats.exercise_3_5

/**
  * 3.5.4 Exercise: Branching out with Functors
  *
  * 1. Implement a `Functor` for the binary tree data type.
  */
import cats._
import cats.implicits._

import FunctorInstances._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}

object FunctorInstances {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(func: A => B): Tree[B] = {
      tree match {
        case Branch(left, right) => Branch(map(left)(func), map(right)(func))
        case Leaf(value)         => Leaf(func(value))
      }
    }
  }
}

object Main extends App {

  val leaf1 = Tree.leaf(10)
  val leaf2 = Tree.leaf(20)
  val branch1 = Tree.branch(leaf1, leaf2)
  println(branch1)

  val branch2 = branch1.map(_ / 2)
  println(branch2)
}
