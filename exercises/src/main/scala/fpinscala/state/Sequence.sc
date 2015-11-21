import fpinscala.state.State
import fpinscala.state.State._

val add2 = State[Int, Int](i => (i, i+2))
val mul5 = State[Int, Int](i => (i, i*5))

val l = List(add2, mul5)
State.sequence(l).run(1)
State.sequence(l.reverse).run(1)

def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
  l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

sequenceViaFoldLeft(l).run(1)
sequenceViaFoldLeft(l.reverse).run(1)

def sequence_it[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
  @annotation.tailrec
  def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
    actions match {
      case Nil => (acc.reverse, s)
      case h :: t => h.run(s) match {
        case (a, s2) => go(s2, t, a :: acc)
      }
    }
  State((s: S) => go(s, sas, List()))
}

sequence_it(l).run(1)
sequence_it(l.reverse).run(1)
