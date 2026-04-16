import fpinscala.exercises.state.State
import State.*

val addAction: State[(Int, Int), Int] =
  for {
    (a, b) <- get
  } yield a + b

addAction.run((3, 5))

val multAction: State[(Int, Int), Int] =
  for {
    (a, b) <- get
  } yield a * b

multAction.run((3, 5))

val incFirst: State[(Int, Int), Unit] =
  for {
    (a, b) <- get
    _ <- set((a + 1, b))
  } yield ()

incFirst.run(3, 5)

val incSecond: State[(Int, Int), Unit] =
  modify { case (a, b) => (a, b + 1) }

incSecond.run(3, 5)

val complexProgram =
  for {
    sum <- addAction
    _ <- incFirst
    _ <- incSecond
    pro <- multAction
  } yield (sum, pro)

complexProgram.run(3, 5)

val moreComplexProgram =
  for {
    (s, p) <- complexProgram
    _ <- set((s, p))
    sum <- addAction
  } yield sum

moreComplexProgram.run(3, 5)

List(2, 3, 4)
  .zip(List(5, 6, 7))
  .map(moreComplexProgram.run(_)._1)
  .sum

def factorialIterative(n:Int): Int = {
  var i = n
  var f = 1
  while i > 0 do {
    f *= i
    i -= 1
  }
  f
}

factorialIterative(5)

val factorialAction: State[(Int, Int), Unit] = {
  // L'estat és la parella (i, f)
  for {
    (i, f) <- get[(Int, Int)]
    _ <-
      if i > 0
      then
        for {
          _ <- set((i - 1, f * i))
          _ <- factorialAction
        } yield ()
      else unit(())
  } yield ()
}

def factorialFriki(n: Int) = {
  // Em quedo amb la segona component de l'estat
  factorialAction.run((n, 1))._2._2
}

factorialFriki(5)