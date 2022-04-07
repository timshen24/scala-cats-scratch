package type_class_intro.my_eq

trait Eq[A] {
  // TODO #1: Define an 'eq' method that takes two A values as parameters, and returns a Boolean
  def eq(first: A, second: A): Boolean
}

object Eq {
  // TODO #2: Define the method 'apply' so we can summon instances from implicit scope
  def apply[A](implicit ev: Eq[A]): Eq[A] = ev

  // TODO #3: Define the method 'instance' so we can build instances of the Eq typeclass more easily.
  //          It should take as the only parameter a function of type (A, A) => Boolean
  def instance[A](f: (A, A) => Boolean): Eq[A] = (first: A, second: A) => f(first, second)

  // TODO #4: Define an Eq instance for String
  implicit val stringEq: Eq[String] = instance(_ == _)

  // TODO #5: Define an Eq instance for Int
  implicit val intEq: Eq[Int] = instance(_ == _)

  // TODO #6: Define an Eq instance for Person. Two persons are equal if both their names and ids are equal.
  //          Extra points: receive implicit instances for String and Int and use them
//  implicit val personEq: Eq[Person] = instance { (p1: Person, p2: Person) =>
//    stringEq.eq(p1.name, p2.name) && intEq.eq(p1.id, p2.id)
//  }

  implicit def personEq(implicit strEq: Eq[String], idEq: Eq[Int]): Eq[Person] =
    (p1: Person, p2: Person) =>
      strEq.eq(p1.name, p2.name) && idEq.eq(p1.id, p2.id)

  // TODO #7: Provide a way to automatically derive instances for Eq[Option[A]] given that we have an implicit
  //          instance for Eq[A]
  implicit def optionEq[A](implicit eq: Eq[A]): Eq[Option[A]] = {
      case (Some(p1), Some(p2)) => eq.eq(p1, p2)
      case (None, None) => true
      case _ => false
    }

  object Syntax {
    // TODO #8: Define a class 'EqOps' with a method 'eqTo' that enables the following syntax:
    //          "hello".eqTo("world")
    implicit class EqOps[A](first: A)(implicit eq: Eq[A]) {
      def eqTo(second: A): Boolean = eq.eq(first, second)
    }
  }
}
