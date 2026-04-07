
def arg[A](a: A): A = {
  println(s"Evaluating $a")
  a
}

def doubleByValue(x: Int): Int = {
  println("First line of double by value")
  x + x
}

doubleByValue(arg(24))

def doubleByName(x: => Int): Int = {
  println("First line of double by name")
  x + x
}

doubleByName(arg(24))

def dontUse(x: => Int): Int = {
  println("First line of dont use")
  42
}

dontUse(arg(24))

def simpleVariable(x: => Int): Int = {
  println("First line of simpleVariable")
  val y = x
  y + y
}

simpleVariable(arg(24))

def doubleByThunk(fx: () => Int): Int = {
  println("First line of double by thunk")
  fx() + fx()
}

doubleByThunk(() => arg(24))

def dontUseWithVariable(x: => Int): Int = {
  println("First line of dont use with variable")
  val y = x
  42
}

dontUseWithVariable(arg(24))

def dontUseWithLazyVariable(x: => Int): Int = {
  println("First line of dont use with lazy variable")
  lazy val y = x
  42
}

dontUseWithLazyVariable(arg(24))

def doubleWithLazyVariable(x: => Int): Int = {
  println("First line of dont use with lazy variable")
  lazy val y = x
  y + y
}

doubleWithLazyVariable(arg(24))

// -----------------------------

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

import LazyList.*

import scala.annotation.tailrec

val l = Cons(() => arg(24), () => Empty)

l.headOption

l.headOption

l.headOption

val ll = cons(arg(24), Empty)

ll.headOption

ll.headOption

ll.headOption

// ----------------------------------

def tabulate[A](n: Int, f: Int => A): LazyList[A] =
  if n == 0 then Empty
  else Cons(() => f(n), () => tabulate(n - 1, f))

def factorial(n: Int): Int = {
  println(s"Computing factorial of $n")
  @tailrec
  def go(n: Int, f: Int): Int = {
    if n == 0 then f else go(n - 1, n * f)
  }
  go(n, 1)
}

val facts = tabulate(10, factorial)

facts.headOption
facts.headOption

def tabulate2[A](n: Int, f: Int => A): LazyList[A] =
  if n == 0 then empty
  else cons(f(n), tabulate(n - 1, f))

val facts2 = tabulate2(10, factorial)

facts2.headOption
facts2.headOption
