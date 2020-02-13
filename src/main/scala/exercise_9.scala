package tkngch.scalawithcats.exercise_9

/**
  * 9 Case Study: Map-Reduce
  */
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats._
import cats.implicits._

object MapReducer {

  def foldMap[A, B: Monoid](input: Vector[A])(func: A => B): B =
    input.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(
      func: A => B
  ): Future[B] = {
    val nBatches: Int = List(Runtime.getRuntime.availableProcessors, 4).min
    val batchSize = (1.0 * values.size / nBatches).round.toInt

    val futures: Iterator[Future[B]] = values
      .grouped(batchSize)
      .map(batch => Future(batch.map(func).foldLeft(Monoid[B].empty)(_ |+| _)))

    Future
      .sequence(futures)
      .map(future => future.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMapCats[A, B: Monoid](
      values: Vector[A]
  )(func: A => B): Future[B] = {
    val nBatches: Int = List(Runtime.getRuntime.availableProcessors, 4).min
    val batchSize = (1.0 * values.size / nBatches).ceil.toInt

    values
      .grouped(batchSize)
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func)))
      .map(_.combineAll)
  }

}

object Main extends App {
  println(
    MapReducer.foldMap(Vector(1, 2, 3))(identity)
  )
  println(
    MapReducer.foldMap(Vector(1, 2, 3))(_.toString + "! ")
  )
  println(
    MapReducer.foldMap("Hello world!".toVector)(_.toString.toUpperCase)
  )

  val future: Future[Int] =
    MapReducer.parallelFoldMap((1 to 100000).toVector)(identity)
  println(Await.result(future, 3.second))

  val futureCats: Future[Int] =
    MapReducer.parallelFoldMapCats((1 to 100000).toVector)(identity)
  println(Await.result(futureCats, 3.second))

}
