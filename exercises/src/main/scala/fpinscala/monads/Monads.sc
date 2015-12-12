import fpinscala.monads.Functor._
import fpinscala.monads.Monad._
import fpinscala.state.State

listFunctor.map(List(1, 2, 3))(_ + 1)

optionMonad.sequence(List(Some(1), Some(2), Some(3)))
optionMonad.sequence(List(Some(1), None, Some(3)))

listMonad.sequence(List(List(1, 2), List(3, 4)))
listMonad.sequence(List(List(1, 2), List()))

optionMonad.traverse(List(1, 2, 3, 4))(n => if (n % 2 == 0) Some(n) else None)
optionMonad.traverse(List(2, 4))(n => if (n % 2 == 0) Some(n) else None)

optionMonad.replicateM(3, Some(1))
listMonad.replicateM(3, List(1, 2))

optionMonad.filterM(List(1, 2, 3, 4))(n => Some(n % 2 == 1))
optionMonad.filterM(List(1, 2, 3, 4))(n => if (n % 2 == 1) Some(true) else None)

listMonad.filterM(List(1, 2, 3, 4))(n => List(n%2 == 1))

listMonad.filterM(List(1))(_ => List(false, false))
listMonad.filterM(List(1))(_ => List(false, true))
listMonad.filterM(List(1))(_ => List(true, false))
listMonad.filterM(List(1))(_ => List(true, true))

listMonad.filterM(List(1, 2))(_ => List(false, false))
listMonad.filterM(List(1, 2))(_ => List(false, true))
listMonad.filterM(List(1, 2))(_ => List(true, false))
listMonad.filterM(List(1, 2))(_ => List(true, true))

val intStateMonad = stateMonad[Int]

val inc = State((i: Int) => (i, i + 1))
val dec = State((i: Int) => (i, i - 1))

intStateMonad.map2(inc, dec)((_, _)).run(1)
intStateMonad.map2(inc, dec)(_ * _).run(1)
intStateMonad.replicateM(4, inc).run(1)
intStateMonad.sequence(List(inc, dec, inc, inc)).run(1)
