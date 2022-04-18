package structure_fp_data.io

object ExampleNotRT {
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

    {
      println("Calculating A")
      a * a
    } + {
      println("Calculating B")
      b * b
    } == {
      println("Calculating C")
      c * c
    }
  }

  def main(args: Array[String]): Unit = {
    println(isPythagorean(3, 4, 5))
  }
}
