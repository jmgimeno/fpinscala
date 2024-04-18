import fpinscala.exercises.state.RNG
import RNG.*

val rng = Simple(42L)

int(rng)

val bool: Rand[Boolean] = map(int)(i => i % 2 == 0)

bool(rng)

enum Direction:
  case North, South, East, West

val direction: Rand[Direction] = map(int)(i => Direction.fromOrdinal(i % 4))

direction(rng)

