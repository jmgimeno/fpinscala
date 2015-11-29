import fpinscala.state.{RNG, State}
import fpinscala.testing._

val rng = RNG.Simple(33)

val gInt = Gen(State(RNG.int))

gInt.sample.run(rng)

val gL5 = Gen.listOfN(5, gInt)

gL5.sample.run(rng)

val succeds = Prop.forAll(gL5)(l => l.reverse.reverse == l)

succeds.run(10, 100, rng)

val fails = Prop.forAll(gL5)(l => l.sum % 2 == 0)

fails.run(10, 100, rng)

(fails && fails).run(10, 100, rng)
(fails && succeds).run(10, 100, rng)
(succeds && fails).run(10, 100, rng)
(succeds && succeds).run(10, 100, rng)

(fails || fails).run(10, 100, rng)
(fails || succeds).run(10, 100, rng)
(succeds || fails).run(10, 100, rng)
(succeds || succeds).run(10, 100, rng)

//val gRange = Gen.choose_basic(1, 7)
val gRange = Gen.choose(1, 7)

gRange.sample.run(rng)

val gLRange = Gen.listOfN(10, gRange)

gLRange.sample.run(rng)

val gU = Gen.union(Gen.choose(1, 7), Gen.choose(11, 17))

val gLU = Gen.listOfN(5, gU)

gLU.sample.run(rng)

val throws = Prop.forAll(Gen.choose(0, 5))(i => 5 % i <= i)

throws.run(10, 100, rng)
val sizedListInt = SGen.listOf(gInt)
sizedListInt.forSize(0).sample.run(rng)
sizedListInt.forSize(1).sample.run(rng)
sizedListInt.forSize(2).sample.run(rng)
sizedListInt.forSize(3).sample.run(rng)
val listOfSize5 = gInt.listOfN(Gen.unit(5))

val unsized = Prop.forAll(listOfSize5)(l => l.length <= 5)
unsized.run(10, 100, rng)

val unsized2 = Prop.forAll(listOfSize5.unsized)(l => l.length <= 5)
unsized2.run(10, 100, rng)

val sized = Prop.forAll(sizedListInt)(l => l.length <= 5)
sized.run(5, 100, rng)
sized.run(10, 100, rng)

