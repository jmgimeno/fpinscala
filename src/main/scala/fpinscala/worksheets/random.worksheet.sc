import fpinscala.exercises.state.*

val rng = RNG.Simple(42L)

rng.nextInt

val ints1 = RNG.ints(2)(rng)
val ints2 = RNG.intsTR(2)(rng)

val l2 = RNG.list(2)
val ints3 = l2(rng)
