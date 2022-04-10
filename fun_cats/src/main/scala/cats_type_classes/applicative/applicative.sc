import cats.Apply

sealed trait Validated[+A]

case class Valid[+A](a: A) extends Validated[A]

case class Invalid(errors: List[String]) extends Validated[Nothing]

def validateName(name: String): Validated[String] =
  if (name.forall(_.isLetter)) Valid(name)
  else Invalid(List("name can only contains letters"))

def validateAge(age: Int): Validated[Int] =
  if (age >= 18) Valid(age)
  else Invalid(List("age must be at least 18"))

case class Person(name: String, age: Int)

def validatePerson(person: Person): Validated[Person] =
  (validateName(person.name), validateAge(person.age)) match {
    case (Valid(_), Valid(_)) => Valid(person)
    case (invalid @ Invalid(_), Valid(_)) => invalid
    case (Valid(_), invalid @ Invalid(_)) => invalid
    case (Invalid(nameErros), Invalid(ageErrors)) => Invalid(nameErros ++ ageErrors)
  }

def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] =
  (va, vb) match {
    case (Valid(a), Valid(b)) => Valid(f(a, b))
    case (invalid @ Invalid(_), Valid(_)) => invalid
    case (Valid(_), invalid @ Invalid(_)) => invalid
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
  }

trait MyApplicatives[F[_]] extends Apply[F] {
  def pure[A](x: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

import cats.implicits._

val f: (Int, Char) => Double = (i, c) => (i + c).toDouble

val int: Option[Int] = Some(5)
val char: Option[Char] = Some('a')
