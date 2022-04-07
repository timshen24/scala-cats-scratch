import java.io.FileOutputStream
import scala.util._

package object type_class_intro {
  def writeToFile(bytes: Array[Byte]): Try[Unit] =
    Using(new FileOutputStream("test_channel.txt")) { os =>
      os.write(bytes); os.flush()
    }
}
