package type_class_intro.my_eq.laws

import type_class_intro.my_eq.Eq

trait EqLaws[A] {
  def eq: Eq[A]

  // #11: Define a 'reflexivity' property which checks that a value is equal to itself
  def reflexivity(a: A): Boolean = eq.eq(a, a)

  // #12: Define a 'symmetry' property which checks that when 'x' is equal to 'y' then 'y' is equal to 'x',
  //           and viceversa
  def symmetry(x: A, y: A): Boolean = eq.eq(x, y) == eq.eq(y, x)

  // #13: Define a 'transitivity' property which checks that if 'x' is equal to 'y' and 'y' is equal to 'z'
  //           then 'x' is equal to 'z'
  //           Hint: The proposition 'IF p THEN q' can be stated as 'NOT p OR q'
  def transitivity(x: A, y: A, z: A): Boolean = !(eq.eq(x, y) && eq.eq(y, z)) || eq.eq(x, z)
}
