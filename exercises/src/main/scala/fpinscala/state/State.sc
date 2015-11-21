import fpinscala.state._
import fpinscala.state.RNG._

val rng = new Simple(-1)

rng.nextInt

nonNegativeInt(rng)

double(rng)

intDouble(rng)

doubleInt(rng)

double3(rng)

ints(3)(rng)

nonNegativeEven(rng)

double_m(rng)

map2(RNG.int, RNG.int)((_, _))(rng)

sequence(List.fill(3)(RNG.int))(rng)

flatMap(RNG.int)(a =>
    map(RNG.int)(b =>
        (a, b)))(rng)

sequence(List.fill(5)(rollDie))(rng)

val inc = State[Int, Int](i => (i, i+1))
val li5 = State.sequence(List.fill(5)(inc))

State.sequence(List.fill(3)(li5)).run(0)

State.sequence_fr(List.fill(3)(li5)).run(0)

val movements = List(Coin, Turn)
val machine = Machine(locked = true, 5, 0)

State.simulateMachine(movements).run(machine)

val movements2 = List(Coin, Coin, Turn)
State.simulateMachine(movements2).run(machine)
