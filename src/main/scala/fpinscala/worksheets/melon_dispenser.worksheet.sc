import fpinscala.exercises.state.*

import Input.*

val inputs = List(Coin, Turn, Coin, Coin, Turn)

val initial = Machine(true, 10, 0)

Candy.simulateMachine(inputs).run(initial)
