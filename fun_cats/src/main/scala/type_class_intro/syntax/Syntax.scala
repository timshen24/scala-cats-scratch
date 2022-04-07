package type_class_intro.syntax

import type_class_intro.syntax.Syntax._
import type_class_intro.using_type_param.{ByteDecoder, ByteEncoder}

import java.nio.ByteBuffer

object Syntax {
  implicit object IntByteDecoder extends ByteDecoder[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] =
      if (bytes.length != 4)
        None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt)
      }
  }

  implicit class ByteEncoderOps[A](val a: A) extends AnyVal {
    def encode(implicit enc: ByteEncoder[A]): Array[Byte] =
      enc.encode(a)
  }

  implicit class ByteDecoderOps[A](val bytes: Array[Byte]) extends AnyVal {
    def decode(implicit dec: ByteDecoder[A]): Option[A] =
      dec.decode(bytes)
  }
}

object Main extends App {
  println(5.encode.mkString("Array(", ", ", ")"))
  println("Hello world".encode.mkString("Array(", ", ", ")"))
  println(Array[Byte](0, 0, 0, 5).decode)
}
