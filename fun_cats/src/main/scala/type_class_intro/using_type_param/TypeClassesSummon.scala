package type_class_intro.using_type_param

//import type_class_intro.using_type_param.ByteEncoder.summon

object TypeClassesSummon extends App {
  // let the compiler find implicit value
  println(ByteEncoder[String].encode("hello").mkString("Array(", ", ", ")"))
  println(ByteDecoder[String].decode(Array(98, 105, 101, 110, 32, 58, 41)))
}
