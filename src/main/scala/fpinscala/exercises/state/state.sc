import fpinscala.exercises.state.State
import State.*

                         // Int => (Unit, Int)
def sum(a: Int, b: Int): State[Int, Unit] =
  for
    _ <- set(a)
    _ <- modify((acc: Int) => acc + b)
  yield ()

sum(3, 4).run(42)

