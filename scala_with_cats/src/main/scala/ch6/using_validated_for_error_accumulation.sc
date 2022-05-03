import cats.data._
import cats.implicits._

import scala.util._
//
//sealed abstract class Either[+A, +B]
//final case class Left[+A, +B](value: A) extends Either[A, B]
//final case class Right[+A, +B](value: B) extends Either[A, B]

sealed trait PersonErrorType

case object NameInvalid extends PersonErrorType

case object AgeInvalid extends PersonErrorType

case object EmailInvalid extends PersonErrorType

final case class PersonError(value: String, errorType: PersonErrorType)

final case class Name(value: String)

final case class Age(value: Int)

final case class Email(value: String)

final case class Person(name: Name, age: Age, email: Email)

type ErrorOr[A] = Either[PersonError, A]

def validateNameWithEither(name: String): ErrorOr[Name] = {
  if (name.headOption.exists(_.isUpper)) Right(Name(name))
  else Left(PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid))
}

def validateAgeWithEither(age: String): ErrorOr[Age] = for {
  numericAge <- Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid))
  validAge <- {
    if (numericAge <= 0 || numericAge > 120) Left(PersonError(s"Age must be a number between 1-120: $numericAge", AgeInvalid))
    else Right(numericAge)
  }
} yield Age(validAge)


def validateEmailWithEither(email: String): ErrorOr[Email] = {
  if (email.isEmpty || !email.contains("@")) Left(PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid))
  else Right(Email(email))
}

def validatePersonWithEither(name: String, age: String, email: String): ErrorOr[Person] = for {
  validName <- validateNameWithEither(name)
  validAge <- validateAgeWithEither(age)
  validEmail <- validateEmailWithEither(email)
} yield Person(validName, validAge, validEmail)

validatePersonWithEither("Benjamin Sisko", "50", "b.sisko@dsn.st")
validatePersonWithEither("odo", "200", "odo.founder.net")

type AllErrorsOr[A] = ValidatedNel[PersonError, A]

def validateNameWithOneRule(name: String): AllErrorsOr[Name] = {
  if (name.headOption.exists(_.isUpper)) Name(name).validNel
  else PersonError(s"Name is empty or does not start with an uppercase character: $name", NameInvalid).invalidNel
}

def validateAge(age: String): AllErrorsOr[Age] = {

  val numericAgeV: AllErrorsOr[Int] = Try(age.toInt).toEither.left.map(ex => PersonError(ex.getMessage, AgeInvalid)).toValidatedNel

  def validAgeV(numericAge: Int): AllErrorsOr[Int] = {
    if (numericAge <= 0 || numericAge > 120) PersonError(s"Age must be a number between 1-120: $numericAge", AgeInvalid).invalidNel
    else numericAge.validNel
  }

  numericAgeV.andThen(validAgeV).map(Age)
}

def validateEmail(email: String): AllErrorsOr[Email] = {
  if (email.isEmpty || !email.contains("@")) PersonError(s"Email address is empty or does not contain an `@` symbol: $email", EmailInvalid).invalidNel
  else Email(email).validNel
}


//A: 1st successful value
//B: 2nd successful value
//Z: Result of applying function `f`
//E: The failure type
//def mapN[Z](f: (A, B) => Z)(implicit functor: Functor[ValidatedNel[E, ?]],implicit semigroupal: Semigroupal[ValidatedNel[E,?]]): ValidatedNel[E,Z]
def validatePersonWithOneRule(name: String, age: String, email: String): AllErrorsOr[Person] = {
  (validateNameWithOneRule(name), validateAge(age), validateEmail(email)).mapN(Person)
}

validatePersonWithOneRule("Benjamin Sisko", "50", "b.sisko@dsn.st")
validatePersonWithOneRule("odo", "200", "odo.founder.net")

def validateNonEmptyName(nameString: String): AllErrorsOr[String] =
  if (nameString.nonEmpty) nameString.validNel else PersonError(s"Name is empty", NameInvalid).invalidNel

def validateStartsWithUpper(nameString: String): AllErrorsOr[String] =
  if (nameString.headOption.exists(_.isUpper)) nameString.validNel else PersonError(s"$nameString does not start with an uppercase character", NameInvalid).invalidNel

def validateNameWithTwoRules(name: String): AllErrorsOr[Name] = {
  validateNonEmptyName(name).andThen(_ => validateStartsWithUpper(name)).map(Name)
}

def validateNameWithProduct(name: String): AllErrorsOr[Name] = {
  (validateNonEmptyName(name) productR validateStartsWithUpper(name)).map(Name)
}

(validateNameWithProduct("Benjamin Sisko"),
  validateAge("50"),
  validateEmail("b.sisko@dsn.st")
  ).mapN(Person)

(validateNameWithProduct("odo"),
  validateAge("200"),
  validateEmail("odo.founder.net")
  ).mapN(Person)

//captures all errors if there are any errors
validateNonEmptyName("") combine validateStartsWithUpper("joe")

//accumulates successes when the success value is a Semigroup
validateNonEmptyName("joe1") combine validateStartsWithUpper("Joe2")

//returns the first validator that succeeds
validateNonEmptyName("joe") combineK  validateStartsWithUpper("Joe2")

//tries the second validator if the first fails
validateStartsWithUpper("joe1") combineK  validateStartsWithUpper("Joe2")

//accumulates errors if all validators fail
validateStartsWithUpper("joe1") combineK  validateStartsWithUpper("joe2")

// And finally you can run a fold on a Validated instance (just like you for Option or Either) to extract the value of failure or success:
validateNonEmptyName("joe").
  fold(failure => s"you failed: $failure", success => s"you succeeded with $success")

import cats.data.Validated._

validateNonEmptyName("joe") match  {
  case Invalid(failure) => s"you failed: $failure"
  case Valid(success) => s"you succeeded with $success"
}