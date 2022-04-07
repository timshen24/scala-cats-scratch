package type_class_intro.using_any

import type_class_intro._

import java.nio.ByteBuffer

trait Channel {
  def write(obj: Any): Unit
}

object FileChannel extends Channel {
  def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int => val bb = ByteBuffer.allocate(4); bb.putInt(n); bb.array()
      case s: String => s.getBytes
      case _ => throw new Exception("unhandled")
    }
    writeToFile(bytes)
  }
}

object Main extends App {
  FileChannel.write("hello")
  println(System.getProperty("user.dir"))
}