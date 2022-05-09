import cats._
import cats.data._
import cats.implicits._
import cats.effect.IO

// Again we use String here for simplicity, in real code this would be something else
type Env = String
type Request = String
type Response = String

def initialEnv: Env = ???

def request(r: Request, env: Env): IO[Response] = ???

def updateEnv(r: Response, env: Env): Env = ???

// We also need some fake requests
def req1: Request = ???
def req2: Request = ???
def req3: Request = ???
def req4: Request = ???

def requestWithState(r: Request): StateT[IO, Env, Response] = for {
  env <- StateT.get[IO, Env]
  resp <- StateT.liftF(request(r, env))
  _ <- StateT.modify[IO, Env](updateEnv(resp, _))
} yield resp

def stateProgram: StateT[IO, Env, Response] = for {
  resp1 <- requestWithState(req1)
  resp2 <- requestWithState(req2)
  resp3 <- requestWithState(req3)
  resp4 <- requestWithState(req4)
} yield resp4

def main: IO[(Env, Response)] = stateProgram.run(initialEnv)