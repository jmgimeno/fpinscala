import fpinscala.exercises.laziness.LazyList.*

val l_eager = LazyList(1, sys.error("boom"), 2)

val l_lazy = cons(1, cons(sys.error("boom"), cons(2, empty)))


