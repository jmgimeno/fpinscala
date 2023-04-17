def factorialIter(n: Int): Int =
  var i = 0
  var f = 1
  while (i < n) do
    i = i + 1
    f = f * i
  f

factorialIter(15)

import fpinscala.exercises.state.State
import State.*

def factorial(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Unit] = ???
  ???
