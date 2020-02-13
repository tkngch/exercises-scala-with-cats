package tkngch.scalawithcats.exercise_2_5

/**
  * 2.5.4 Exercise: Adding All The Things
  *
  * Ex.1: SuperAdder
  * 1. Implement a method, `def add(items: List[Int]): Int` for adding numbers.
  *
  * Ex.2:
  * 1. Change `add` so that we can add `List[Option[Int]]`.
  *
  * Ex.3:
  * 1. Without making changes to `add`, implement a method to add up `Orders`:
  * ```
  * case class Order(totalCost: Double, quantity: Double)
  * ```
  */
import cats._
import cats.implicits._

object SuperAdder1 {
  def add(items: List[Int]): Int = items.fold(0)(_ + _)
}

object SuperAdder2 {
  def add[A](items: List[A])(implicit m: Monoid[A]): A =
    items.fold(m.empty)(_ |+| _)
}

case class Order(val totalCost: Double, val quantity: Double)

object MonoidInstances {
  implicit val orderMon: Monoid[Order] = new Monoid[Order] {
    def combine(order1: Order, order2: Order) =
      Order(
        totalCost = order1.totalCost + order2.totalCost,
        quantity = order1.quantity + order2.quantity
      )
    def empty = Order(totalCost = 0, quantity = 0)
  }
}

object Main extends App {
  import MonoidInstances._
  println(SuperAdder1.add(List(1, 2, 3)))
  println(SuperAdder2.add(List(Some(1), Some(2), Some(3), None)))
  println(SuperAdder2.add(List(Order(10, 20), Order(2, 5))))
}
