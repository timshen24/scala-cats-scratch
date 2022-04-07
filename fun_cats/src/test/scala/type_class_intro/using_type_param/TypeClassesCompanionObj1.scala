package type_class_intro.using_type_param

import type_class_intro._
import type_class_intro.using_type_param.ByteEncoder.instance

import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(a: String): Array[Byte] = a.getBytes
  }

  implicit object IntByteEncoder extends ByteEncoder[Int] {
    override def encode(a: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(a)
      bb.array()
    }
  }

  // helper method
  def /*summon*/ apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

//  implicit val stringByteEncoder: ByteEncoder[String] = (s: String) =>
//    s.getBytes()

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)

  implicit val stringByteEncoder: ByteEncoder[String] = instance(_.getBytes)
}

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
  def read[A](bytes: Array[Byte])(implicit dec: ByteDecoder[A]): Option[A]
}

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder {
  implicit object StringByteDecoder extends ByteDecoder[String] {
    override def decode(bytes: Array[Byte]): Option[String] =
      Some(new String(bytes))
  }

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = (bytes: Array[Byte]) => f(bytes)

  implicit val stringByteDecoder: ByteDecoder[String] = instance[String]((bytes: Array[Byte]) => Some(new String(bytes)))
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)
    writeToFile(bytes)
  }

  override def read[A](bytes: Array[Byte])(implicit dec: ByteDecoder[A]): Option[A] = dec.decode(bytes)
}

object Main extends App {
  // Direct scope has precedence over Companion object
  /*  implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
    override def encode(a: String): Array[Byte] = a.getBytes.map(b => (b + 3).toByte)
  }*/
  implicit val rot3StringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes.map(b => (b + 3).toByte))
  FileChannel.write[Int](5)
  FileChannel.write[String]("abcd")
  println(System.getProperty("user.dir"))
}
