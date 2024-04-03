import fpinscala.exercises.laziness.LazyList

import LazyList.*

def f(n: Int) =
  println(s"f $n")
  n

val left = cons(f(1), cons(f(2), empty))
val right = cons(f(3), cons(f(4), empty))

val all = left.append(right)

def sum(lazyList: LazyList[Int]): Int =
  lazyList match
    case LazyList.Empty => 0
    case LazyList.Cons(h, t) => h() + sum(t())

all.headOption

sum(all)
