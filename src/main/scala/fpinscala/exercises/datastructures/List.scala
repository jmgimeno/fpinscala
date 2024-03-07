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

  /*
    foldRight(List(a1, a2, a3), acc, f)
  =
   f(a1, f(a2, f(a3, acc)))
                    ^^^^^^^^^
           ^^^^^^^^^^^^^^^
   ^^^^^^^^^^^^^^^^^^^^
   */

  def length[A](l: List[A]): Int =
    l match
      case Nil         => 0
      case Cons(x, xs) => 1 + length(xs)

  def lengthViaFoldRight[A](l: List[A]): Int =
    foldRight(l, 0, (_, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil         => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  /*
    foldLeft(List(a1, a2, a3), acc, f)
  =
   f(f(f(acc, a1), a2), a3)
        ^^^^^^^^^
      ^^^^^^^^^^^^^^^
    ^^^^^^^^^^^^^^^^^^^^^
   */

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, _) => 1 + x)

  def reverse[A](l: List[A]): List[A] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => append(reverse(tail), Cons(head, Nil))

  def snoc[A](init: List[A], last: A): List[A] =
    init match
      case Nil         => Cons(last, Nil)
      case Cons(a, as) => Cons(a, snoc(as, last))

  def reverse2[A](l: List[A]): List[A] =
    l match
      case Nil              => Nil
      case Cons(head, tail) => snoc(reverse2(tail), head)

  /*
  snoc(List(1, 2, 3), 4) => List(1, 2, 3, 4)

  Cons(1, List(2, 3, 4)) => List(1, 2, 3, 4)
   */

  def reverseTailrec[A](l: List[A]): List[A] =
    @annotation.tailrec
    def go(l: List[A], acc: List[A]): List[A] =
      l match
        case Nil              => acc
        case Cons(head, tail) => go(tail, Cons(head, acc))
    go(l, Nil)

  /*
    reverse(List(1, 2, 3))
  =
    go(List(1, 2, 3), Nil)
  =
    go(List(2, 3), List(1))
  =
    go(List(3), List(2, 1))
  =
    go(List(), List(3, 2, 1))
  =
    List(3, 2, 1)
   */

  def reverseViaFoldRight[A](l: List[A]): List[A] =
    foldRight(
      l,
      Nil: List[A],
      (head, reversedTail) => append(reversedTail, Cons(head, Nil))
    )

  def reverseViaFoldRight2[A](l: List[A]): List[A] =
    foldRight(
      l,
      Nil: List[A],
      (head, reversedTail) => snoc(reversedTail, head)
    )

  /*
    def reverseTailrec[A](l: List[A]): List[A] =
      @annotation.tailrec
      def go(l: List[A], acc: List[A]): List[A] =
        l match
          case Nil              => acc
          case Cons(head, tail) => go(tail, Cons(head, acc))
      go(l, Nil)

    @annotation.tailrec
    def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
      l match
        case Nil         => acc
        case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
   */

  def reverseViaFoldLeft[A](l: List[A]): List[A] =
//    val f = (acc: List[A], a: A) => Cons(a, acc)

//    val f = new Function2[List[A], A, List[A]]:
//      override def apply(acc: List[A], a: A): List[A] =
//        Cons(a, acc)

    foldLeft(l, Nil: List[A], (acc, a) => Cons(a, acc))

  /*
    reverseViaFoldLeft(List(1, 2, 3))
  =
    foldLeft(List(1, 2, 3), Nil, (acc, a) => Cons(a, acc))
  =
    foldLeft(List(2, 3), Cons(1, Nil), (acc. a) => Cons(a, acc))
  =
    foldLeft(List(3), Cons(2, Cons(1, Nil)), (acc, a)=>Cons(a, acc))
  =
    foldLeft(Nil, Cons(3, Cons(2, Cons(1, Nil))), (acc, a) => Cons(a, acc)
  =
    Cons(3, Cons(2, Cons(1, Nil)))
   */

  /*
    foldRight(List(a1, a2, a3), acc, f)
  =
   f(a1, f(a2, f(a3, acc)))
                    ^^^^^^^^^
           ^^^^^^^^^^^^^^^
   ^^^^^^^^^^^^^^^^^^^^

    foldLeft(List(a1, a2, a3), acc, f)
  =
   f(f(f(acc, a1), a2), a3)
        ^^^^^^^^^
      ^^^^^^^^^^^^^^^
    ^^^^^^^^^^^^^^^^^^^^^
   */

  /*
    foldLeft(reverse(List(a1, a2, a3), acc, g)
  =
   foldLeft(List(a3, a2, a1), acc, g)
  =
   g(g(g(acc, a3), a2), a1)

  g = (acc, a) => f(a, acc)

  g(g(g(acc, a3), a2), a1)
  =
  f(a1, g(g(acc, a3), a2))
  =
  f(a1, f(a2, g(acc, a3)))
  =
  f(a1, f(a2, f(a3, acc)))

    foldRight(List(a1, a2, a3), acc, f)
  =
   f(a1, f(a2, f(a3, acc)))
   */

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverseViaFoldLeft(l), acc, (acc, a) => f(a, acc))

  /*
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)((h, append_t_a2) => Cons(h, append_t_a2))

  def appendViaFoldRight2[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)(Cons(_, _))

  def appendViaFoldRight3[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)(Cons.apply)

  // linear on total length of lists
  // with foldLeft the cost is not linear
  def concat[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((as, acc) => append(as, acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((element, acc) =>
      Cons(element + 1, acc)
    )

  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((element, acc) =>
      Cons(element.toString, acc)
    )

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((element, acc) =>
      Cons(f(element), acc)
    )

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A]) { (element, acc) =>
      if f(element) then Cons(element, acc)
      else acc
    }

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B]) { (element, acc) =>
      append(f(element), acc)
    }

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(
      as,
      a =>
        if f(a) then Cons(a, Nil)
        else Nil
    )

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
      case _                          => Nil

  // def zipWith - TODO determine signature

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case _                          => Nil

  def zipWithTailRec[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    @annotation.tailrec
    def go(a: List[A], b: List[B], acc: List[C]): List[C] =
      (a, b) match
        case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x, y), acc))
        case _                          => acc
    reverseViaFoldLeft(go(a, b, Nil: List[C]))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

end List
