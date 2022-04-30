import cats.data.State

final case class Robot (
                         id: Long,
                         sentient: Boolean,
                         name: String,
                         model: String
                       )

final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

val initialSeed = Seed(13L)

val nextLong: State[Seed, Long] = State(seed => (seed.next, seed.long))
val nextBoolean: State[Seed, Boolean] = nextLong.map(long => long >= 0)

val createRobot: State[Seed, Robot] = {
  val b = nextBoolean
  for {
    id <- nextLong
    sentient <- b
    isCatherine <- b
    name = if (isCatherine) "Catherine" else "Carlos"
    isReplicant <- b
    model = if (isReplicant) "replicant" else "borg"
  } yield Robot(id, sentient, name, model)
}

val (finalState, robot) = createRobot.run(initialSeed).value