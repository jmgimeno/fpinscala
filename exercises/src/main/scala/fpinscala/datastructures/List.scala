package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("List.tail of Nil does not exist")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("List.setHead on Nil is not possible")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n > 0) drop(t, n - 1) else l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, t) => go(t, f(acc, h))
    }
    go(l, z)
  }

  // Exercise 11
  val sum_fl = (l: List[Int]) => List.foldLeft(l, 0)(_ + _)

  val product_fl = (l: List[Int]) => List.foldLeft(l, 1)(_ * _)

  def length_fl[A](l: List[A]) = List.foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 12
  def reverse_fl[A](l: List[A]) = List.foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  // Exercise 13
  def foldRight_fl[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse_fl(as), z)((b, a) => f(a, b))

  def foldRight_fl_2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, identity[B](_))((fbb, a) => b => fbb(f(a, b)))(z)

  def foldLeft_fr[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, identity[B](_))((a, fbb) =>  b => fbb(f(b, a)))(z)

  // Exercise 14
  def append_fr[A](as1: List[A], as2: List[A]) = List.foldRight(as1, as2)(Cons(_, _))

  // Exercise 15
  def flatten_fr[A](ass: List[List[A]]): List[A] = List.foldRight(ass, Nil: List[A])(append)

  // Exercise 16
  def inc_each(is: List[Int]): List[Int] =
    foldRight(is, Nil:List[Int])((i,acc) => Cons(i+1, acc))

  // Exercise 17
  def toString_each(is: List[Int]): List[String] = 
    foldRight(is, Nil:List[String])((i, acc) => Cons(i.toString, acc))

  // Exercise 18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  // Exercise 20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, acc) => append(f(a), acc))

  def flatMap_2[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten_fr(map(as)(f))

  // Exercise 21
  def filter_fm[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 22
  def add_lists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add_lists(t1, t2))
  }

  // Exercise 23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
