package tkngch.scalawithcats.exercise_4_6

/**
  * 4.6.5 Exercise: Safer Folding using Eval
  * 1. Implement stack safe `foldRight` using `Eval`.
  */
import cats._

object Ops {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(
          fn(head, foldRight(tail, acc)(fn))
        )
      case Nil => acc
    }

  def foldRightEval2[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

}

object Main extends App {
  println(Ops.foldRight(List.range(0, 500), 0)(_ + _))

  println(
    Ops
      .foldRightEval(List.range(0, 500), Eval.now(0))((num1, num2) =>
        Eval.now(num1 + num2.value)
      )
      .value
  )
  println(Ops.foldRightEval2(List.range(0, 500), 0)(_ + _))
}
