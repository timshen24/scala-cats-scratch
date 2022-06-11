package essentialeffect.ch3

import cats.effect._
import cats.implicits._
import essentialeffect.DebugHelper

object ParMapN extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = par.as(ExitCode.Success)

  val hello: IO[String] = IO("hello").debug
  val world: IO[String] = IO("world").debug

  val par: IO[String] = (hello, world).parMapN((h, w) => s"$h $w").debug
}
