import fpinscala.exercises.laziness.LazyList
import LazyList.*

val l_eager: LazyList[Int] = LazyList(1, sys.error("boom"), 2)

val l_lazy: LazyList[Int] = cons(1, cons(sys.error("boom"), cons(2, empty)))

def f(n: Int): Int =
  println(s"f $n")
  n

val lazyList = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))
val normalList = f(1) :: f(2) :: f(3) :: f(4) :: Nil

lazyList.headOption

lazyList.headOption

val firstThree = lazyList.take(3)

firstThree.tailOption

firstThree.tailOption

firstThree.tailOption.flatMap(_.headOption)

val lazyList2 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

val afterThree = lazyList2.drop(3)

afterThree.headOption

val lazyList3 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

val untilThree = lazyList3.takeWhile(_ < 3)

untilThree.headOption

untilThree.tailOption
