import cats.data._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

case class Cache[K, V](data: Map[K, V]) {
  def put(key: K, value: V): Cache[K, V] = Cache(data + (key -> value))

  def get(key: K): Option[V] = data.get(key)

  def remove(key: K): Cache[K, V] = Cache(data - key)
}

type Log = List[String]
type UUID = String

case class User()
case class Uri()
case class MyHttpClient() {
  def get(uri: Uri): IO[Option[User]] = ???
}
case class MyConfig(userUuids: List[UUID])

case class Environment(httpClient: MyHttpClient, config: MyConfig)

def getConfig: MyConfig = ???

def getUser(
             uuid: UUID
           ): ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Option[User]] =
  ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Option[User]] {
    case (env, cache) =>
      env.httpClient.get(Uri()).map {
        case Some(user) =>
          (List(s"Found a user $user"), cache.put(uuid, user), Some(user))
        case None =>
          (List(s"User with id $uuid doest not exist in the system"), cache, None)
      }
  }

val result = for {
  _ <- getUser("user1")
  _ <- getUser("user3")
} yield ()

val (logs, userCache, _) = result
  .run(Environment(MyHttpClient(), getConfig), Cache(Map.empty))
  .unsafeRunSync()

def preCacheUsers(
                   userUuids: List[UUID]
                 ): ReaderWriterStateT[IO, Environment, Log, Cache[UUID, User], Unit] =

  userUuids match {
    case Nil =>
      ReaderWriterStateT.pure[IO, Environment, Log, Cache[UUID, User], Unit]()

    case currentUserUuid :: rest =>
      for {
        _ <- ReaderWriterStateT.tell[IO, Environment, Log, Cache[UUID, User]](
          List(s"Pre caching user $currentUserUuid"))
        _ <- getUser(currentUserUuid)
        result <- preCacheUsers(rest)
      } yield result
  }

val result = for {
  environment <- ReaderWriterStateT.ask[IO, Environment, List[String], Cache[UUID, User]]
  _ <- preCacheUsers(environment.config.userUuids)
} yield ()

val (logs, newState, _) = result
  .run(Environment(MyHttpClient(), getConfig), Cache(Map.empty))
  .unsafeRunSync()