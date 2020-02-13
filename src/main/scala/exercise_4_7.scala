package tkngch.scalawithcats.exercise_4_7

/**
  * 4.7.3 Exercise: Show Your Working
  * 1. Implement `factorial` such that the log messages are captured in a `Writer`.
  */
import cats.data.Writer
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._ // for tell
import cats.instances.vector._ // for Monoid

object Ops {
  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logger[A] = Writer[Vector[String], A]

  def factorialLogged(n: Int): Logger[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logger]
      } else {
        slowly(factorialLogged(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

}

object Main extends App {
  val baseline = Ops.factorial(5)
  val logged = Ops.factorialLogged(5)

  println(logged)
}
