package type_class_intro.box

import cats.laws.discipline.MonadTests
import cats_type_classes.box.Box
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import cats.kernel.laws.discipline.EqTests

class BoxSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  val genInt: Gen[Int] = Gen.choose(1, 10)
  val genInt2: Gen[Int] = Gen.oneOf(1, 5, 10)
  val genString: Gen[String] = Gen.alphaNumStr
  val genString2: Gen[String] = Gen.numStr
  val genTuple: Gen[(Int, String)] =
    for {
      i <- genInt
      s <- genString
    } yield(i , s)

  val arbInt: Arbitrary[Int] = Arbitrary(genInt)

  implicit def arbBoxA[A](implicit arbA: Arbitrary[A]): Arbitrary[Box[A]] =
    Arbitrary(arbA.arbitrary.map(Box.apply))

  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(arbA.arbitrary.map(a => (_: A) => a))

  checkAll("Eq[Box[Int]]", EqTests[Box[Int]].eqv)
  checkAll("Monad[Box]", MonadTests[Box].monad[Int, Int, Int])
}
