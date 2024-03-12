import fpinscala.exercises.errorhandling.Option

import Option.*

def f(n: Int): Int =
  println("Cridant a la funció")
  n + 1

val opt = Some(1)

val result = opt.getOrElse(f(25))
