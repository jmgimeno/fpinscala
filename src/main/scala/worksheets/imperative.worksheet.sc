import fpinscala.answers.state.State
import State.*

val program: State[Int, Unit] =
  for
    i <- get
    _ <- set(i + 1)
    i2 <- get
    _ <- set(i2 * 2)
  yield ()

program.run(1)
program.run(2)

val program2: State[Int, Unit] =
  for
    _ <- modify((i: Int) => i + 1)
    _ <- modify((i: Int) => i * 2)
  yield ()

program2.run(1)
program2.run(2)

def mkListImperative(n: Int): List[Int] =
  var i = n
  var l = List.empty[Int]
  while i != 0 do
    l = i :: l
    i -= 1
  val r = l
  r

mkListImperative(5)

def mkListFunctional(n: Int): List[Int] =
  lazy val mkListAction: State[(Int, List[Int]), List[Int]] =
    for
      (i, l) <- get[(Int, List[Int])]
      _ <-
        if i != 0
        then
          for
            _ <- set((i - 1, i :: l))
            _: List[Int] <- mkListAction
          yield ()
        else
          unit(())
      (_, r) <- get
    yield r
  mkListAction.run((n, List.empty))._1

mkListFunctional(5)

def mkReversedListFunctional(n: Int) : List[Int] =
  lazy val mkReversedListAction: State[Int, List[Int]] =
    for
      i <- get[Int]
      l <-
        if i == 0
        then unit(List.empty)
        else
          for
            _ <- set(i - 1)
            l1 <- mkReversedListAction
          yield i :: l1
    yield l
  mkReversedListAction.run(n)._1

mkReversedListFunctional(5)


