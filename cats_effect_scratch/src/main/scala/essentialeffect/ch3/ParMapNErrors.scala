package essentialeffect.ch3

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import essentialeffect.DebugHelper

import scala.concurrent.duration.DurationInt

object ParMapNErrors extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = e1.attempt.debug >>
    IO("---").debug >>
    e2.attempt.debug >>
    IO("---").debug >>
    e3.attempt.debug >>
    IO("---").debug >>
    good.attempt.debug >>
    IO.pure(ExitCode.Success)

  val ok: IO[String] = IO("hi").debug
  val name: IO[String] = IO("boys").debug
  val ko1: IO[String] = IO.sleep(1.seconds) >> IO.raiseError[String](new RuntimeException("oh!")).debug
  val ko2: IO[String] = IO.raiseError[String](new RuntimeException("noes!")).debug

  // parMapN === parTupled
//  val e1: IO[Unit] = (ok, ko1).parMapN((_, _) => ())
  val e1: IO[Unit] = (ok, ko1).parTupled.void
//  val e2: IO[Unit] = (ko1, ok).parMapN((_, _) => ())
  val e2: IO[Unit] = (ko1, ok).parTupled.void
//  val e3: IO[Unit] = (ko1, ko2).parMapN((_, _) => ())
  val e3: IO[Unit] = (ko1, ko2).parTupled.void
  val good: IO[(String, String)] = (ok, name).parTupled
}
