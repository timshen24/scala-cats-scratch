package type_class_intro.derivation

import type_class_intro.using_type_param.ByteEncoder._
import type_class_intro.using_type_param._

object Derivation {
//  implicit object OptionString extends ByteEncoder[Option[String]] {
//    override def encode(a: Option[String]): Array[Byte] = a match {
//      case Some(value) => StringByteEncoder.encode(value)
//      case None        => Array[Byte]()
//    }
//  }
//
//  implicit object OptionInt extends ByteEncoder[Option[Int]] {
//    override def encode(a: Option[Int]): Array[Byte] = a match {
//      case Some(value) => IntByteEncoder.encode(value)
//      case None        => Array[Byte]()
//    }
//  }
  implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = {
    case Some(value) => encA.encode(value)
    case None        => Array[Byte]()
  }
}

object Main extends App {
  import Derivation._
  println(ByteEncoder[String].encode("hello").mkString("Array(", ", ", ")"))
  println(ByteEncoder[Int].encode(1000).mkString("Array(", ", ", ")"))
  println(ByteEncoder[Option[String]].encode(Some("world")).mkString("Array(", ", ", ")"))
  println(ByteEncoder[Option[String]].encode(None).mkString("Array(", ", ", ")"))
  println(ByteEncoder[Option[Int]].encode(Some(1000)).mkString("Array(", ", ", ")"))
  println(ByteEncoder[Option[Int]].encode(None).mkString("Array(", ", ", ")"))
}
