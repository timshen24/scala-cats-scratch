package essentialeffect.ch3

import cats.effect._
import cats.implicits._
import essentialeffect.DebugHelper

object ParTraverse extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    tasks
      .parTraverse(task)
      .debug
      .as(ExitCode.Success)

  val numTasks = 100
  val tasks: List[Int] = List.range(0, numTasks)

  def task(id: Int): IO[Int] = IO(id).debug
}
