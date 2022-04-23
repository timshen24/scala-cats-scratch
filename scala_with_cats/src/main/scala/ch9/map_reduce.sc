import cats._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

def foldMap[A, B: Monoid](vector: Vector[A])(f: A => B): B =
//  vector.map(f).combineAll
    vector.foldLeft(Monoid[B].empty)(_ |+| f(_))

foldMap(Vector(1, 2, 3))(_.toString + "! ")
foldMap("Hello world".toVector)(_.toString.toUpperCase)

def parallelFoldMap[A, B: Monoid](vector: Vector[A])(f: A => B): Future[B] = {
  val numCores = Runtime.getRuntime.availableProcessors()
  val groupSize = ((vector.size * 1.0) / numCores).ceil.toInt
  val futures = vector.grouped(groupSize).map(group => Future(foldMap(group)(f)))
  Future.sequence(futures).map {iterable => iterable.foldLeft(Monoid.empty[B])(_ |+| _)}
}

val f = parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(f, 1.second)

def parallelFoldMap2[A, B: Monoid](vector: Vector[A])(f: A => B): Future[B] = {
  val numCores = Runtime.getRuntime.availableProcessors()
  val groupSize = ((vector.size * 1.0) / numCores).ceil.toInt

  // Vector[Vector[A]]--->traverse(Vector[A]====foldMap===>Future[B])--->Future[Vector[B]]--->combineAll--->Future[B]
  vector.grouped(groupSize).toVector.traverse(group => Future(group.foldMap(f))).map(_.combineAll)
}

val f = parallelFoldMap2((1 to 1000000).toVector)(identity)
Await.result(f, 1.second)