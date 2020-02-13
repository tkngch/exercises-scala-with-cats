package tkngch.scalawithcats.exercise_4_8

/**
  * 4.8.3 Exercise: Hacking on Readers
  * 1. Create a type alias `DbReader` for a `Reader` which consumes a `Db` as input.
  * 2. Implement methods which generate `DbReader`s to look up the username given the user
  * ID and the password given a username.
  * 3. Create a `checkLogin` method to check the password for a given user ID.
  */
import cats.data.Reader
import cats.syntax.all._

case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

object ReaderOps {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordCorrect <- username
        .map(name => checkPassword(name, password))
        .getOrElse(false.pure[DbReader])
    } yield passwordCorrect
}

object Main extends App {
  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  val check1 = ReaderOps.checkLogin(1, "zerocool").run(db)
  println(check1)
  val check2 = ReaderOps.checkLogin(4, "davinci").run(db)
  println(check2)
}
