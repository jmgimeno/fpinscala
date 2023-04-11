import fpinscala.exercises.laziness.LazyList

import LazyList.*

def f(n: Int): Int =
  println(s"computing $n")
  n

val lCached = cons(f(1), cons(f(2), empty))
val lNoCached = Cons(() => f(1), () => Cons(() => f(2), () => Empty))

lCached.headOption

lCached.headOption

lNoCached.headOption

lNoCached.headOption

val ll = LazyList(f(1), f(2), f(3))

ll.headOption

val list = lCached.toList
lCached.toList

val list2 = lNoCached.toList
lNoCached.toList

val llong = cons(f(1), cons(f(2), cons(f(3), cons(f(4), cons(f(5), empty)))))

val llong3 = llong.take(3)

llong3.toList

val llong2 = llong3.take(2)

llong2.toList

val longNew = cons(f(1), cons(f(2), cons(f(3), cons(f(4), cons(f(5), empty)))))

// description
val result = longNew.drop(2).take(2)

// evaluation
result.toList.length

LazyList.fibsViaUnfold.take(10).toList

// Primer parcial 2023 problema 5

val facts = LazyList.unfold((0, 1)) { case (n, f) =>
  Some((f, (n + 1, (n + 1) * f)))
}

facts.take(10).toList
