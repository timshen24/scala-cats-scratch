package ch11

import cats.kernel.CommutativeMonoid
import cats.implicits._

final case class RawGCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): RawGCounter = {
    val count = counters.getOrElse(machine, 0) + amount
    RawGCounter(counters + (machine -> count))
  }

  def merge(that: RawGCounter): RawGCounter = {
    RawGCounter(that.counters ++ this.counters.map {
      case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
    })
  }

  def total: Int = counters.values.sum
}

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1 max a2

    override def empty: Int = 0
  }

  implicit def setInstance[A](): BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 | a2

    override def empty: Set[A] = Set.empty[A]
  }
}

final case class GeneralGCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GeneralGCounter[A] = {
    val count = counters.getOrElse(machine, m.empty) |+| amount
    GeneralGCounter(counters + (machine -> count))
  }

  def merge(that: GeneralGCounter[A])(implicit b: BoundedSemiLattice[A]): GeneralGCounter[A] = {
    GeneralGCounter(this.counters |+| that.counters)
  }

  def total(implicit m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
}