import fpinscala.state.{RNG, State}
import fpinscala.testing._

val rng = RNG.Simple(33)

val gInt = Gen(State(RNG.int))

gInt.sample.run(rng)

val gL5 = gInt.listOfN(5)

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

val gLRange = gRange.listOfN(10)

gLRange.sample.run(rng)

val gU = Gen.union(Gen.choose(1, 7), Gen.choose(11, 17))

val gLU = gU.listOfN(5)

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
// Using library and improving usability
import fpinscala.testing.Prop._
import fpinscala.testing.Gen._
import fpinscala.testing.SGen._
val smallInt = choose(-10, 10)
val maxProp = forAll(listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists {_ > max}
}
run(maxProp)
// Exercise 14
val sortedProp = forAll(listOf(smallInt)) { ns =>
  def isSorted(ns: List[Int]): Boolean = ns match {
    case i1 :: i2 :: l => (i1 <= i2) && isSorted(i2 :: l)
    case _ => true
  }
  isSorted(ns.sorted)
}
run(sortedProp)

