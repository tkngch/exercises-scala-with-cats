package tkngch.scalawithcats.exercise_4_9

/**
  * 4.9.3 Exercise: Post-Order Calculator
  */
import cats.data.State
import cats.syntax.applicative._ // for pure

object Ops {
  type CalcState[A] = State[List[Int], A]

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (first, second) =>
      first.flatMap(_ => evalOne(second))
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operate(_ + _)
      case "-" => operate(_ - _)
      case "*" => operate(_ * _)
      case "/" => operate(_ / _)
      case numberString =>
        State[List[Int], Int] { state =>
          val number = numberString.toInt
          (number :: state, number)
        }
    }

  def operate(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case first :: second :: tail =>
        val ans = func(first, second)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail!")
    }
}

object Main extends App {
  val program = for {
    _ <- Ops.evalOne("1")
    _ <- Ops.evalOne("2")
    ans <- Ops.evalOne("+")
  } yield ans

  println(program)
  println(program.runA(Nil).value)

  val program2 = Ops.evalAll(List("1", "2", "+", "3", "*"))
  println(program2.runA(Nil).value)

  println(Ops.evalInput("1 2 + 3 4 + *"))
}
