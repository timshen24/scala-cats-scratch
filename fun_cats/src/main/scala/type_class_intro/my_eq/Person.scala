package type_class_intro.my_eq

case class Person(name: String, id: Int)

object Person {
  object Instances {
    // TODO #9: Define an Eq instance for Person comparing them by name
    //          Extra points: receive an implicit instance for String and use it
    implicit def eqPersonName(implicit stringEq: Eq[String]): Eq[Person] =
      (p1: Person, p2: Person) => stringEq.eq(p1.name, p2.name)

    // TODO #10: Define an Eq instance for Person comparing them by id
    //           Extra points: receive an implicit instance for Int and use it
    implicit def eqPersonId(implicit idEq: Eq[Int]): Eq[Person] =
      (p1: Person, p2: Person) => idEq.eq(p1.id, p2.id)
  }
}