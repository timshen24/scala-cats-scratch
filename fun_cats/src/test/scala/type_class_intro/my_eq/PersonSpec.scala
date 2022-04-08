package type_class_intro.my_eq

import type_class_intro.my_eq.laws.EqLaws
import type_class_intro.my_eq.laws.discipline.EqTests

class PersonSpec extends MySpec {
  // #17: Write tests for additional Eq instances defined in Person using
  //           Discipline and the 'checkAll' method
  checkAll("Eq[Person] by name", EqTests(Person.Instances.eqPersonName).eq)
  checkAll("Eq[Person] by id", EqTests(Person.Instances.eqPersonId).eq)
}
