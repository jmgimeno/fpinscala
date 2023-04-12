import fpinscala.exercises.state.*

val rng = RNG.Simple(42L)

rng.nextInt

val ints1 = RNG.ints(2)(rng)
val ints2 = RNG.intsTR(2)(rng)

//  def list[A](rand: Rand[A])(count: Int): Rand[List[A]] =
val genD5 = RNG.list(RNG.double)(5)

genD5(rng)

List.fill(3)("patata")

val ints3 = RNG.intsViaSequence(RNG.int)(2)(rng)
