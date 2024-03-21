import fpinscala.exercises.laziness.LazyList
import LazyList.*

val l_eager: LazyList[Int] = LazyList(1, sys.error("boom"), 2)

val l_lazy: LazyList[Int] = cons(1, cons(sys.error("boom"), cons(2, empty)))


