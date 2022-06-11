package essentialeffect.ch3

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import essentialeffect.DebugHelper

object ParSequence extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    // parSequence就是parTraverse的identity版本
    // x.sequence == x.traverse(identity)
    // x.traverse(f) == x.map(f).sequence
    tasks.parSequence
      .debug
      .as(ExitCode.Success)
  }

  val numTasks = 10
  val tasks: List[IO[Int]] = List.tabulate(numTasks)(task)
  def task(id: Int): IO[Int] = IO(id).debug
}
