import cats._
import cats.implicits._

5.valid[List[String]]
"error".invalid[Int]

import cats.data._
5.valid[NonEmptyList[String]]
5.validNel[String]
"error".invalidNel[Int]

def concat[A](as: List[A], as2: List[A]): List[A] = {
  as.foldLeft(as2)((b, a) => b :+ a)
  as.foldRight(as2)((a, b) => a :: b)
  // time inefficiency
}
concat(List(1, 2, 3), List(4))

// Non Empty Chain -- concat faster, not linear on the first list
/**
 * Chain is a data structure that allows constant time prepending and appending.
 * This makes it especially efficient when used as a Monoid, e.g. with Validated or Writer.
 * As such it aims to be used where List and Vector incur a performance penalty.
 */
5.validNec[String]
"error".invalidNec[Int]

// fail on number is not even
5.validNec[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)
6.validNec[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)

Validated.cond(test = true, 5, "error")
Validated.condNec(test = false, 5, "error")

5.validNec[String].getOrElse(10)
"error".invalidNec[Int].getOrElse(10)

5.validNec[String].orElse(10.validNec[String])
"error".invalidNec[Int].orElse(10.validNec[String])

5.validNec[String].toEither
"error".invalidNec[Int].toEither

Validated.fromEither[NonEmptyChain[String], Int](Right(5))
Validated.fromEither[NonEmptyChain[String], Int](Left(NonEmptyChain("error")))