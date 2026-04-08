import fpinscala.exercises.laziness.LazyList
import LazyList.*

def arg[A](a: A): A =
  println(s"Evaluating $a")
  a

val l = cons(arg(1), cons(arg(2), cons(arg(3), cons(arg(4), empty))))

val t = l.take(2)

t.headOption

val s = t.tailOption

s.get.headOption

///

val ll = cons(arg(1), cons(arg(2), cons(arg(3), cons(arg(4), empty))))

def multi(n: Int): LazyList[Int] = {
  def go(i: Int): LazyList[Int] = {
    println(s"go $i of $n")
    if i == 0 then LazyList.empty
    else LazyList.cons(n * 10, go(i - 1))
  }
  println(s"Applying multi to $n")
  go(n)
}

val fm = ll.flatMap(multi)

fm.tailOption.get.tailOption.get.tailOption.get.headOption

////

continually("patata").take(3).toList

from(25).take(4).toList

fibs.take(10).toList