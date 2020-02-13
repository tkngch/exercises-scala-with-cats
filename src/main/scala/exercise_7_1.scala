package tkngch.scalawithcats.exercise_7_1

/**
  * 7.1.2 Exercise: Reflecting on Folds
  *
  * 7.1.3 Exercise: Scaf-fold-ing Other Methods
  */
import scala.math.Numeric

object Substitutes {
  def map[A, B](input: List[A])(func: A => B): List[B] =
    input.foldRight(List.empty[B])((item, accum) => func(item) :: accum)

  def flatMap[A, B](input: List[A])(func: A => List[B]): List[B] =
    input.foldRight(List.empty[B])((item, accum) => func(item) ::: accum)

  def filter[A](input: List[A])(func: A => Boolean): List[A] =
    input.foldRight(List.empty[A])((item, accum) =>
      if (func(item)) item :: accum else accum
    )

  def sum[A](input: List[A])(implicit num: Numeric[A]): A =
    input.foldRight(num.zero)(num.plus)
}

object Main extends App {

  // accum is List[Int], item is Int
  println(
    List(1, 2, 3, 4, 5).foldLeft(List.empty[Int])((accum, item) =>
      accum ::: List(item)
    )
  )

  println(
    List(1, 2, 3, 4, 5).foldRight(List.empty[Int])((item, accum) =>
      item :: accum
    )
  )

}
