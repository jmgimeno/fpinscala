import fpinscala.exercises.errorhandling.Option

import Option.*

def f(n: Int): Int =
  println("Cridant a la funció")
  n + 1

val opt = Some(1)

val result = opt.getOrElse(f(25))

val options: List[Option[String]] = List(Some("a"), None, Some("c"))

val res: Option[List[String]] = Option.sequence(options)

val data = List(1, 2, 3, 4, 5)

def onlyEven(n: Int): Option[Int] =
  if (n % 2 == 0)
  then Some(n * 4 + 3)
  else None

val allOk = Option.traverse(data)(onlyEven)

val someOk = data.map(onlyEven)
