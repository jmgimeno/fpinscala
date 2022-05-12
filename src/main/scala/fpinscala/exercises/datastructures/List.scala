package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x: Int = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  // NOTE: The cost of append is linear on the length of a1. The length of a2 is irrelevant !!!
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => sys.error("set head of empty list")
      case Cons(_, t) => Cons(h, t)

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case Nil => Nil
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _ => l

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  // def foldRight[A,B](l: List[A], acc: B, f: (A, B) => B): B
  // foldRight substitutes Cons <-> f and Nil <-> acc
  // Cons(90, Cons(56, Cons(23, Nil)))
  //  f  (90,  f  (56, f (23, acc))))
  //        (1+      (1+      (1 +     0)))
  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  // List(x1, x2, x3)
  // Cons(x1, Cons(x2, Cons(x3, Nil)))
  //      x   -----------------------
  //                  xs
  // If we call foldLeft with xs we need to pass f(acc, x) as the next value of the accumulator
  // fl:   f(f(f(acc, x1), x2), x3)
  //       f(f(      acc', x2), x3)
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
    }

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  // (acc, x) => new acc
  // x1, x2, x3 -> x3, x2, x1
  // The first element of the list will be the last element of the reversed list
  def reverse[A](l: List[A]): List[A] =
    foldLeft[A, List[A]](l, Nil, (acc, x) => Cons(x, acc))
  //         ^^^^^^^^^^ needed to help the compiler to check type
  // Alternative: foldLeft(l, Nil:List[A], (acc, x) => Cons(x, acc))

  // foldRight substitutes Cons <-> Cons, and Nil <-> r
  // Cons(x1, Cons(x2,                                ))
  //                   Cons(y1, Cons(y2, Cons(y3, Nil)))
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons.apply)

  // List(List(1, 2), List(2, 3, 4), List(5, 6, 7))
  // append(List(1, 2), append(List(2, 3, 4), append(List(5, 6, 7), Nil))
  // NOTE: we cam do the same with foldLeft but now the cost is quadratic instead of linear
  // append(append(append(Nil, List(1, 2)), List(2, 3, 4)), List(5, 6, 7))
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A], append)

  // Cons(x1, Cons(x2, Cons(x3, Nil)))
  //   f (x1,   f (x2,    f(x3, acc))) <- foldRight on list
  //   g(g(g(acc, x1), x2), x3) <- foldLeft on list
  //   g(g(g(acc, x3), x2), x1) <- foldLeft on reversed list
  // If we reverse the list, going left to right is "the same" as goint right to left on the
  // original list
  def foldRightViaFoldLeft[A,B](l: List[A], acc:B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc, (acc, x) => f(x, acc))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int], (i, acc) => Cons(i+1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String], (i, acc) => Cons(i.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B], (i,acc) => Cons(f(i),acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A], (i,acc) => if f(i) then Cons(i, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    // concat(map(as, f))
    foldRight(as, Nil:List[B], (a, acc) => append(f(a), acc))

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    @annotation.tailrec
    def go(a: List[Int], b: List[Int], acc: List[Int]): List[Int] =
      (a, b) match {
        case (Cons(ah, at), Cons(bh, bt)) => go(at, bt, Cons(ah+bh, acc))
        case _ => acc
    }
    reverse(go(a, b, Nil))

  def zipWith[A,B,C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    @annotation.tailrec
    def go(a: List[A], b: List[B], acc: List[C]): List[C] =
      (a, b) match {
        case (Cons(ah, at), Cons(bh, bt)) => go(at, bt, Cons(f(ah, bh), acc))
        case _ => acc
      }
    reverse(go(a, b, Nil))

  // addPairWise(a, b) = zipWith(a, b, _ + _)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
