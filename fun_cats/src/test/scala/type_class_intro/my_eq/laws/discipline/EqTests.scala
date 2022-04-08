package type_class_intro.my_eq.laws.discipline

import type_class_intro.my_eq.Eq
import type_class_intro.my_eq.laws.EqLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait EqTests[A] extends Laws {
  def laws: EqLaws[A]

  // #14: Define a RuleSet containing the laws in EqLaws
  def eq(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
    name = "eq",
    parent = None,
    "reflexivity" -> forAll(laws.reflexivity _),
    "symmetry" -> forAll(laws.symmetry _),
    "transitivity" -> forAll(laws.transitivity _)
  )
}

// #15: Define a companion object with an 'apply' method so that we can
//           easily instatiate tests with e.g. EqTests[Int]
object EqTests {
  def apply[A](implicit eqA: Eq[A]): EqTests[A] = new EqTests[A] {
    override def laws: EqLaws[A] = new EqLaws[A] {
      override def eq: Eq[A] = eqA
    }
  }
}