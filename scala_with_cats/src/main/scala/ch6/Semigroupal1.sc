import cats.Semigroupal
import cats.instances.option._

Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))
Semigroupal[Option].product(Some(123), None)

Semigroupal.tuple3(Option(1), Option(2), Option(3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)

import cats.syntax.apply._

(Option(123), Option("abc")).tupled
(Option(123), Option("abc"), Option(true)).tupled

final case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & Black")
  ).mapN(Cat.apply)

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.string._

final case class Cat(
                      name: String,
                      yearOfBirth: Int,
                      favoriteFoods: List[String]
                    )

val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
val catToTuple: Cat => (String, Int, List[String]) =
  cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

implicit val catMonoid: Monoid[Cat] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._ // for |+|

val garfield = Cat("Garfield", 1978, List("Lasagne"))
val healthcliff = Cat("Healthcliff", 1988, List("Junk Food"))
garfield |+| healthcliff

import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent._
import scala.concurrent.duration._

val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
Await.result(futurePair, 1.second) // for mapN

case class Cat(
                name: String,
                yearOfBirth: Int,
                favoriteFoods: List[String]
              )
val futureCat = (
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
).mapN(Cat.apply)

Await.result(futureCat, 1.second)
Semigroupal[List].product(List(1, 2), List(3, 4))
type ErrorOr[A] = Either[Vector[String], A]

Semigroupal[ErrorOr].product(
  Left(Vector("Error 1")),
  Left(Vector("Error 2"))
)

type ErrorOr[A] = Either[Vector[String], A]
val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))
Semigroupal[ErrorOr].product(error1, error2)

import cats.syntax.parallel._

//(error1, error2).tupled
(error1, error2).parTupled

type ErrorOr[A] = Either[List[String], A]
val errStr1: ErrorOr[Int] = Left(List("error1"))
val errStr2: ErrorOr[Int] = Left(List("error2"))
(errStr1, errStr2).parTupled

val success1: ErrorOr[Int] = Right(1)
val success2: ErrorOr[Int] = Right(2)
val addTwo = (x: Int, y: Int) => x + y
(error1, error2).parMapN(addTwo)
(success1, success2).parMapN(addTwo)

//trait Parallel[M[_]] {
//  type F[_]
//  def applicative: Applicative[F]
//  def monad: Monad[M]
//  def parallel: ~->[M, F]
//}

(List(1, 2), List(3, 4)).parTupled