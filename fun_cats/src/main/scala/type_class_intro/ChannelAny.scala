package type_class_intro

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

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

    Using(new FileOutputStream("test_channel.txt")) {
      os => os.write(bytes); os.flush()
    }
  }
}

object Main extends App {
  FileChannel.write("hello")
  println(System.getProperty("user.dir"))
}