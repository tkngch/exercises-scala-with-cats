package tkngch.scalawithcats.exercise_5_4

/**
  * 5.4 Exercise: Monads: Transform and Roll Out
  */
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import cats.data._
import cats.instances.future._ // for Monad

object MonadTransformers {
  type Response[A] = Future[Either[String, A]]
  type ResponseT[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): ResponseT[Int] = {
    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )
    val powerLevel = powerLevels.get(autobot)
    powerLevel match {
      case Some(level) => EitherT.right(Future(level))
      case None        => EitherT.left(Future(s"$autobot's power-level is unknown."))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): ResponseT[Boolean] = {
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield (powerLevel1 + powerLevel2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 10.second) match {
      case Left(msg)    => s"Error: $msg"
      case Right(true)  => s"$ally1 and $ally2 can perform a special move!"
      case Right(false) => s"$ally1 and $ally2 cannot perform a special move."
    }
  }

}

object Main extends App {
  println(MonadTransformers.tacticalReport("Jazz", "Bumblebee"))
  println(MonadTransformers.tacticalReport("Bumblebee", "Hot Rod"))
  println(MonadTransformers.tacticalReport("Jazz", "Ironhide"))
}
