package type_class_intro.using_type_param

case class Switch(isOn: Boolean)

// also workable if defined a companion object for Switch case class
object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(a: Switch): Array[Byte] =
      Array(if(a.isOn) '1'.toByte else '0'.toByte)
  }
}

object Main2 extends App {
  FileChannel.write[Switch](Switch(true))
}
