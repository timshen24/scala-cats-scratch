def isPythagorean(a: Int, b: Int, c: Int): Boolean = {
  val c2 = {
    println("Calculating C")
    c * c
  }

  val b2 = {
    println("Calculating B")
    b * b
  }

  val a2 = {
    println("Calculating A")
    a * a
  }

  a2 + b2 == c2
}

isPythagorean(3, 4, 5)