import type_class_intro.using_type_param.ByteEncoder
import type_class_intro.using_type_param.ByteEncoder._

implicitly[ByteEncoder[String]].encode("hello")