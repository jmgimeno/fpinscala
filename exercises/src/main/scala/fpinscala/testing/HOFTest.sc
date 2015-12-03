import fpinscala.state.{RNG, State}
import fpinscala.testing.{SGen, Gen, Prop}

val int = Gen(State(RNG.int))

val isEven = (i: Int) => i % 2 == 0

val takeWhileProp =
  Prop.forAll(SGen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))

Prop.run(takeWhileProp)

