package essentialeffect.ch3

import cats.effect._
import cats.implicits._
import essentialeffect.DebugHelper

object DebugExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = seq.as(ExitCode.Success)

  val hello: IO[String] = IO("hello").debug
  val world: IO[String] = IO("world").debug

  val seq: IO[String] = (hello, world).mapN((h, w) => s"$h $w")
    .debug
}
