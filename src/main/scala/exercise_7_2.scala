package tkngch.scalawithcats.exercise_7_2

/**
  * 7.2.2.1 Exercise: Traversing with Vectors
  * 7.2.2.2 Exercise: Traversing with Options
  * 7.2.2.3 Exercise: Traversing with Validated
  */
import scala.language.higherKinds
import cats._
import cats.implicits._
import cats.data.Validated

object Ops {
  def listTraverse[F[_]: Applicative, A, B](
      list: List[A]
  )(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }
}

object Main extends App {
  println(Ops.listSequence(List(Vector(1, 2), Vector(3, 4))))
  println(Ops.listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))
  println(Ops.process(List(2, 4, 6)))
  println(Ops.process(List(1, 2, 3)))
  println(Ops.processValidated(List(2, 4, 6)))
  println(Ops.processValidated(List(1, 2, 3)))
}
