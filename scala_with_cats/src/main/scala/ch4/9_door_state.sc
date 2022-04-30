import cats.Eval
import cats.data.IndexedStateT

sealed trait DoorState
case object Open extends DoorState
case object Closed extends DoorState

case class Door(state: DoorState)

def open: IndexedStateT[Eval, Closed.type, Open.type, Unit] = IndexedStateT.set(Open)
def close: IndexedStateT[Eval, Open.type, Closed.type, Unit] = IndexedStateT.set(Closed)

val valid: IndexedStateT[Eval, Closed.type, Open.type, Unit] = for {
  _ <- open
  _ <- close
  _ <- open
} yield ()

//valid.run(Open).value
valid.run(Closed).value