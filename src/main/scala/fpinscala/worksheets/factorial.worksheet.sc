import fpinscala.exercises.state.State
import State.*

def incrementBy(n: Int): State[Int, Unit] =
  State { s =>
    ((), s + n)
  }

def multiplyBy(n: Int): State[Int, Unit] =
  State.modify(_ * n)

val program =
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

def factorialIter(n: Int): Int =
  var i = 0
  var f = 1
  while (i < n) do
    i = i + 1
    f = f * i
  f

factorialIter(15)

def factorial(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Unit] =
    get[FactorialState].flatMap { fs =>
      if (fs.i < n) then
        set(FactorialState(fs.i + 1, fs.f * (fs.i + 1))).flatMap { _ =>
          factorialAction
        }
      else unit(())
    }
  factorialAction.run(FactorialState(0, 1))._2.f

factorial(15)

def factorial2(n: Int) =
  case class FactorialState(i: Int, f: Int)
  lazy val factorialAction: State[FactorialState, Int] =
    get[FactorialState].flatMap { fs =>
      if (fs.i < n) then
        set(FactorialState(fs.i + 1, fs.f * (fs.i + 1))).flatMap { _ =>
          factorialAction
        }
      else unit(fs.f)
    }
  factorialAction.run(FactorialState(0, 1))._1

factorial2(15)
