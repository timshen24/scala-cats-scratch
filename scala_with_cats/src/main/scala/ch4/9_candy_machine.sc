import cats.data.State

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

def singleInput(input: Input): State[Machine, Unit] =
  State [Machine, Unit] {
    machine => (input, machine) match {
      case (_, Machine(_, 0, _)) => (machine, ())
      case (Coin, Machine(false, _, _)) => (machine, ())
      case (Turn, Machine(true, _, _)) => (machine, ())
      case (Coin, Machine(true, candy, coin)) =>
        val machine = Machine(locked = false, candy, coin + 1)
        (machine, ())
      case (Turn, Machine(false, candy, coin)) =>
        val machine = Machine(locked = true, candy - 1, coin)
        (machine, ())
    }
  }

def simulateMachine(inputs: List[Input]): State[Machine, Unit] =
  inputs.map(singleInput).reduce{(prev, cur) => prev.flatMap(_ => cur)}

singleInput(Coin).run(Machine(locked = true, 1, 1)).value
singleInput(Turn).run(Machine(locked = false, 1, 2)).value
simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(locked = true, 1, 1)).value
simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).runS(Machine(locked = true, 5, 10)).value