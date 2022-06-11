package essentialeffect.ch3

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import essentialeffect.DebugHelper

object ParTraverse2 extends IOApp {
  def f(i: Int): IO[Int] = IO(i)

  override def run(args: List[String]): IO[ExitCode] =
    (f(1), f(2)).parMapN((a, b) => List(a, b)).debug >>
      (f(1), f(2), f(3)).parMapN((a, b, c) => List(a, b, c)).debug >>
      (f(1), f(2), f(3), f(4)).parMapN((a, b, c, d) => List(a, b, c, d)).debug >>
      List(1, 2, 3, 4).parTraverse(f).debug >> IO.pure(ExitCode.Success)
}
