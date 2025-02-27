package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
   * which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    // case Cons(x, _) if x == 0.0 => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result: Int = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("setHead of empty list")
    case Cons(_, tail) => Cons(h, tail)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil => Nil
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case list@Cons(_, _) => list

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case list@Cons(_, _) => list

  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))

  // -------------------------------------

  def length[A](l: List[A]): Int =
    //    l match
    //    case List.Nil => 0
    //    case List.Cons(_, tail          ) =>  1 + length(tail)
    foldRight(l, 0, (_, length_of_tail) => 1 + length_of_tail)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case List.Nil => acc
    case List.Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def foldLeftIterative[A, B](l: List[A], acc: B, f: (B, A) => B): B =

    def head(l: List[A]): A = l match
      case List.Nil => sys.error("head of empty list")
      case List.Cons(head, _) => head

    var result: B = acc
    var lst: List[A] = l
    while lst != List.Nil do
      val current: A = head(lst)
      result = f(result, current)
      lst = tail(lst)
    result

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, (sum_of_initial_part, current) =>
      sum_of_initial_part + current
    )

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, (product_of_initial_part, current) =>
      product_of_initial_part * current
    )

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (length_of_initial_part, _) =>
      length_of_initial_part + 1
    )

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List.Nil: List[A], (result_reverse_init, head) =>
      List.Cons(head, result_reverse_init)
    )

  // NOTES IMPORTANTS:
  // Recordeu els problemes de recursivitat de Programació 2
  // - resoldre pel prefix -> foldLeft
  // - resoldre pel sufix -> foldRight
  //
  // El foldRight original aplica la f a la tornada de les crides; la recursivitat serveix només
  // per tenir a la pila els elements en l'ordre invertit; per tant, les crides recursives (des de
  // la llista inicial fins a la buida) fan el mateix que el reverse, i la tornada d'aquestes,
  // que va aplicant les f's en ordre invers dels elements, el mateix que fa el foldLeft sobre la
  // invertida.
  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc, (resultat_init, head) =>
      f(head, resultat_init)
    )

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, List.Cons.apply)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List.Nil: List[A], (head, result_concat_tail) =>
      append(head, result_concat_tail)
    )
  // with foldLeft is possible but quadratic number of copies (with foldRight is linear)
  // foldLeft(l, List.Nil: List[A],(result_concat_init, head) => append(result_concat_init, head)

  def incrementEach(l: List[Int]): List[Int] = l match
    case List.Nil => List.Nil
    case List.Cons(head, tail) => List.Cons(head + 1, incrementEach(tail))

  def incrementEachViaFoldRight(l: List[Int]): List[Int] =
    foldRight(l, List.Nil: List[Int], (head, increment_tail) =>
      List.Cons(head + 1, increment_tail)
    )

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List.Nil: List[String], (head, doubleToString_tail) =>
      List.Cons(head.toString, doubleToString_tail)
    )

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, List.Nil: List[B], (head, map_tail) =>
      List.Cons(f(head), map_tail)
    )

  def incrementViaMap(l: List[Int]): List[Int] = map(l, _ + 1)

  def doubleToStringViaMap(l: List[Double]): List[String] = map(l, _.toString)

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List.Nil: List[A], (head, filter_tail) =>
      if f(head)
      then List.Cons(head, filter_tail)
      else filter_tail
    )

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List.Nil: List[B], (head, flatMap_tail) =>
      append(f(head), flatMap_tail)
    )

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (a: A) =>
      if f(a)
      then List.Cons(a, Nil)
      else List.Nil
    )

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (List.Cons(ha, ta), List.Cons(hb, tb)) => List.Cons(ha + hb, addPairwise(ta, tb))
    case _ => List.Nil

  // def zipWith - TODO determine signature
  def zipWith[A,B,C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match
    case (List.Cons(ha, ta), List.Cons(hb, tb)) => List.Cons(f(ha, hb), zipWith(ta, tb, f))
    case _ => List.Nil

  def addPairWiseViaZipWith(a: List[Int], b: List[Int]): List[Int] =
    zipWith(a, b, _ + _)

  // HARD:
  // - These implementations have only theoretical interest to deepen the understanding of folds,
  // recursion and functions.
  // - Hint: in both solutions we "grow" a function that, given the value of acc gets the result

  def foldRightViaFoldLeftAlt[A, B](l: List[A], acc: B, f: (A, B) => B): B = ???

  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B, A) => B): B = ???

  // Hint: use an auxiliar function to determine if sub is a prefix of a list.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
