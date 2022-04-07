package type_class_intro.using_inheritance

import type_class_intro._

trait Channel {
  def write(obj: ByteEncodable): Unit
}

trait ByteEncodable {
  def encode(): Array[Byte]
}

case class FullName(firstName: String, lastName: String) extends ByteEncodable {
  override def encode(): Array[Byte] =
    firstName.getBytes ++ lastName.getBytes
}

object FileChannel extends Channel {
  def write(obj: ByteEncodable): Unit = {
    val bytes: Array[Byte] = obj.encode()
    writeToFile(bytes)
  }
}

object Main extends App {
  FileChannel.write(FullName("hello", "world"))
  println(System.getProperty("user.dir"))
}
