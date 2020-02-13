package tkngch.scalawithcats.exercise_6_3

/**
  * 6.3.1.1 Exercise: The Product of Monads
  */
import cats._
import cats.implicits._

object Ops {
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(value => y.map((value, _)))
}

object Main extends App {
  println(Ops.product(List(1, 2), List(3, 4)))
}
