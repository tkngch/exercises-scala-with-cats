package tkngch.scalawithcats.exercise_4_1

/**
  * 4.1.2 Exercise: Getting Func-y
  * 1. Define `map` using `flatMap` and `pure`.
  */
import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(func andThen pure)
}

object Main extends App {
  //
}
