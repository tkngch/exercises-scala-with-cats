package tkngch.scalawithcats.exercise_6_4

/**
  * 6.4.4 Exercise: Form Validation
  */
import cats.data._
import cats.implicits._

case class User(name: String, age: Int)

object Ops {
  type Data = Map[String, String]
  type EitherValue[A] = Either[List[String], A]
  type ValidatedValue[A] = Validated[List[String], A]

  def createUser(input: Data): ValidatedValue[User] =
    (readName(input).toValidated, readAge(input).toValidated).mapN(User.apply)

  def readName(input: Map[String, String]): EitherValue[String] =
    getValue("name", input).flatMap(nonBlank)

  def readAge(input: Map[String, String]): EitherValue[Int] =
    getValue("age", input)
      .flatMap(nonBlank)
      .flatMap(parseInt)
      .flatMap(nonNegative)

  def getValue(
      fieldName: String,
      input: Map[String, String]
  ): EitherValue[String] =
    input.get(fieldName).toRight(List(s"Invalid field name: $fieldName"))

  def parseInt(value: String): EitherValue[Int] =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => List(s"Integer expected. Found $value instead."))

  def nonBlank(value: String): EitherValue[String] =
    Right(value).ensure(List("Blank string received."))(_.nonEmpty)

  def nonNegative(value: Int): EitherValue[Int] =
    Right(value).ensure(List("Negative integer received."))(_ >= 0)

}

object Main extends App {
  val invalidName = Ops.getValue("invalid", Map("name" -> "John"))
  println(invalidName)
  val name = Ops.getValue("name", Map("name" -> "John"))
  println(name)

  val invalidAge = Ops.parseInt("1O")
  println(invalidAge)
  val age = Ops.parseInt("10")
  println(age)

  println(Ops.readName(Map("name" -> "John Smith")))
  println(Ops.readName(Map()))
  println(Ops.readAge(Map("age" -> "100")))
  println(Ops.readAge(Map("age" -> "twenty")))

  println(Ops.createUser(Map("name" -> "John Smith", "age" -> "74")))
  println(Ops.createUser(Map("name" -> "John Smith", "age" -> "thirty")))
  println(Ops.createUser(Map("name" -> "", "age" -> "-10")))
}
