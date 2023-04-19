import fpinscala.exercises.state.State
import State.*

def incrementBy(n: Int): State[Int, Unit] =
  State { s =>
    ((), s + n)
  }

def multiplyBy(n: Int): State[Int, Unit] =
  State.modify(_ * n)

  //
  // for
  //   s <- get
  //   _ <- set(s * n)
  // yield ()
  //
  // get.flatMap { s =>
  //   set(s * n).map { _ =>
  //     ()
  //   }
  // }

val program: State[Int, Unit] =
  for
    _ <- incrementBy(12)
    _ <- multiplyBy(2)
  yield ()

program.run(5)

val program2 =
  for
    n <- get[Int]
    _ <- set(n + 12)
    n2 <- get
    _ <- set(n2 * 2)
  yield ()

program2.run(5)

val program3 =
  get[Int].flatMap { n =>
    set(n + 12).flatMap { _ =>
      get.flatMap { n2 =>
        set(n2 * 2).map { _ =>
          ()
        }
      }
    }
  }

program3.run(6)

val otherProgram =
  for
    _ <- set(5)
    _ <- incrementBy(12)
    _ <- multiplyBy(2)
    r <- get
  yield r

otherProgram.run(4)

def factorialIter(n: Int): Int =
  var i = 0
  var f = 1
  while (i < n) do
    i = i + 1
    f = f * i
  f

factorialIter(15)

def factorialTR(n: Int): Int =
  @annotation.tailrec
  def loop(i: Int, f: Int): Int =
    if (i < n) then
      val (newI, newF) = (i + 1, f * (i + 1))
      loop(newI, newF)
    else f
  loop(0, 1)

factorialTR(15)

def factorial(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Unit] =
    get[FactorialState].flatMap { case FactorialState(i, f) =>
      if (i < n) then
        set(FactorialState(i + 1, f * (i + 1))).flatMap { _ =>
          factorialAction
        }
      else unit(())
    }
  factorialAction.run(FactorialState(0, 1))._2.f

factorial(15)

def factorial2(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Int] =
    get[FactorialState].flatMap { case FactorialState(i, f) =>
      if (i < n) then
        set(FactorialState(i + 1, f * (i + 1))).flatMap { _ =>
          factorialAction
        }
      else unit(f)
    }
  factorialAction.run(FactorialState(0, 1))._1

factorial2(15)

def factorial3(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Int] =
    get.flatMap { case FactorialState(i, f) =>
      if (i < n) then
        set(FactorialState(i + 1, f * (i + 1)))
          *> factorialAction
      else unit(f)
    }
  factorialAction.run(FactorialState(0, 1))._1

factorial3(15)
