package tkngch.scalawithcats.exercise_4_3

/**
  * 4.3.1 Exercise: Monadic Secret Identities
  * 1. Implement `pure`, `map`, and `flatMap` for `Id`.
  */
import cats.Id

object MonadIdOps {
  def pure[A](value: A): Id[A] = value
  def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
}

object Main extends App {
  println("John": Id[String])
  val id1 = MonadIdOps.pure(42)
  println(id1)
  val id2 = MonadIdOps.map(id1)(_ * 2)
  println(id2)
  val id3 = MonadIdOps.flatMap(id2)(_ * 2)
  println(id3)
}
