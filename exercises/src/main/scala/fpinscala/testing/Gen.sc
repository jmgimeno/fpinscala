import fpinscala.state.{RNG, State}
import fpinscala.testing._

val rng = RNG.Simple(33)

val gInt = Gen(State(RNG.int))

gInt.sample.run(rng)

val gL5 = Gen.listOfN(5, gInt)

gL5.sample.run(rng)

val succeds = Prop.forAll(gL5)(l => l.reverse.reverse == l)

succeds.run(100, rng)

val fails = Prop.forAll(gL5)(l => l.sum % 2 == 0)

fails.run(100, rng)

(fails && fails).run(100, rng)
(fails && succeds).run(100, rng)
(succeds && fails).run(100, rng)
(succeds && succeds).run(100, rng)

(fails || fails).run(100, rng)
(fails || succeds).run(100, rng)
(succeds || fails).run(100, rng)
(succeds || succeds).run(100, rng)

//val gRange = Gen.choose_basic(1, 7)
val gRange = Gen.choose(1, 7)

gRange.sample.run(rng)

val gLRange = Gen.listOfN(10, gRange)

gLRange.sample.run(rng)

val gU = Gen.union(Gen.choose(1, 7), Gen.choose(11, 17))

val gLU = Gen.listOfN(5, gU)

gLU.sample.run(rng)

val throws = Prop.forAll(Gen.choose(0, 5))(i => 5 % i <= i)

throws.run(100, rng)
