import fpinscala.answers.state.State
import State.*

// program (està amagat): Int => (Unit, Int)
val program: State[Int, Unit] =
  for
    i <- get
    _ <- set(i + 1)
    i2 <- get
    _ <- set(i2 * 2)
  yield ()


program.run(1)
// col·locant com a valor inicial de l'estat s0 = 1
// i <- get
//     - get = s => (s, s)
// i val el mateix que s0 => i = 1
// set(s: S) = _ => ((), s)
// i + 1 = 2
// set(i + 1) col·locar com a nou valor de l'estat s1 = 2
// i2 <- get
// i2 sigui el valor actul de l'estat s1 = 2
// set(i2*2) -> col·locar com a varlor de l'estat s2 = 4
// => ((), 4)

program.run(2)

val megaprogram: State[Int, Unit] =
  val patata = program
  for
    _ <- patata
    _ <- patata
  yield ()

megaprogram.run(5)
// i = 5
// s1 = 6
// i2 = 6
// s2 = 12
// i = 12
// s3 = 13
// i2 = 13
// s4 = 26


val program2: State[Int, Unit] =
  for
    _ <- modify((i: Int) => i + 1)
    _ <- modify((i: Int) => i * 2)
  yield ()

program2.run(1)
program2.run(2)

/*
List<Integer> mkListJava(int n) {
  int i = n;
  var l = new LinkedList<Integer>();
  while (i != 0) {
    l.addFirst(i);
    i -= 1;
  }
  return l;
}
*/

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
  //                ((Int, List[Int])) => (List[Int], (Int, List[Int]))
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
      (_, r) <- get[(Int, List[Int])]
    yield r
  mkListAction.run((n, List.empty))._1

mkListFunctional(5)

def mkListFunctional2(n: Int) : List[Int] =
  lazy val mkReversedListAction: State[Int, List[Int]] =
    for
      i <- get[Int]
      l <-
        if i > n
        then unit(List.empty)
        else
          for
            _ <- set(i + 1)
            l1 <- mkReversedListAction
          yield i :: l1
    yield l
  mkReversedListAction.run(1)._1

mkListFunctional2(5)

var increment: State[Int, Unit] = State.modify((i: Int) => i + 1)
var increments: List[State[Int, Unit]] = List.fill(100)(increment)
var increment100: State[Int, List[Unit]] = sequence(increments)

increment100.run(50)._2
