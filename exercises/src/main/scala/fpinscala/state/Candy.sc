import fpinscala.state._
import fpinscala.state.State._

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (update andThen modifyState))
    s <- getState
  } yield (s.coins, s.candies)
}

val movements = List(Coin, Turn)
val machine = Machine(locked = true, 5, 0)

Candy.simulateMachine(movements).run(machine)


