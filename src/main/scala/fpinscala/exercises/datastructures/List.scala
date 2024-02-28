package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil

  /** Another data constructor, representing nonempty lists. Note that `tail` is
    * another `List[A]`, which may be `Nil` or another `Cons`.
    */
  case Cons(head: A, tail: List[A])
end List

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match // A function that uses pattern matching to add up a list of integers
      case Nil         => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
        x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  /** tots els elements menys el primer */
  def tail[A](l: List[A]): List[A] = l match
    case Nil           => sys.error("tail of empty list")
    case Cons(_, tail) => tail

  /** Canviar el primer element d'una llista per el que es passa per parametre
    */
  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil           => sys.error("can't swap first element of [Nothing]")
      case Cons(_, tail) => Cons(h, tail)

  // elimina els n primers elements de la llista
  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case Nil                  => Nil
      case Cons(_, xs) if n > 0 => drop(xs, n - 1)
      case _                    => l

  // mentre cumpleixi la condicio elimina elements
  def dropWhile[A](l: List[A], cond: A => Boolean): List[A] =
    l match
      case Nil                    => Nil
      case Cons(x, xs) if cond(x) => dropWhile(xs, cond)
      case _                      => l

  // Llista amb tots els elements excepte el darrer
  // errors sobre llista buida
  def init[A](l: List[A]): List[A] =
    l match
      case Nil          => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))

  def foldRight[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(
      ns,
      1.0,
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def length[A](l: List[A]): Int = ???
  //
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = ???

  def sumViaFoldLeft(ns: List[Int]): Int = ???

  def productViaFoldLeft(ns: List[Double]): Double = ???

  def lengthViaFoldLeft[A](l: List[A]): Int = ???

  def reverse[A](l: List[A]): List[A] = ???

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = ???

  def concat[A](l: List[List[A]]): List[A] = ???

  def incrementEach(l: List[Int]): List[Int] = ???

  def doubleToString(l: List[Double]): List[String] = ???

  def map[A, B](l: List[A], f: A => B): List[B] = ???

  def filter[A](as: List[A], f: A => Boolean): List[A] = ???

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = ???

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

end List
