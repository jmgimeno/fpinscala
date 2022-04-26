import fpinscala.exercises.testing.*
import fpinscala.answers.state.*

import Gen.*

val g1 = Gen.choose(0, 5)
val g2 = Gen.choose(10, 15)
val u = Gen.union(g1, g2)

val rng = RNG.Simple(1L)

g1.listOfN(10).sample(rng)
g2.listOfN(10).sample(rng)
Gen.union(g1, g2).listOfN(10).sample(rng)

g1.sample(rng)
g2.listOfN(g1).sample(rng)

//val res2: List[Int] = List(3, 11, 4, 2, 14, 14, 2, 4, 0, 10)
val w = Gen.weighted((g1, 0.8), (g2, 0.2))

w.listOfN(10).sample(rng)
