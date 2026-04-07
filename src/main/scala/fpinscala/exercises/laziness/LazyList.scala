package fpinscala.exercises.laziness

import scala.annotation.tailrec

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match {
    case LazyList.Empty => Nil
    case LazyList.Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match {
    case LazyList.Cons(h, t) if n > 0 => LazyList.cons(h(), t().take(n - 1))
    case _ => LazyList.empty
  }

  @tailrec
  final def drop(n: Int): LazyList[A] = this match {
    case LazyList.Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case LazyList.Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
    case _ => LazyList.empty
  }

  def takeWhile_foldRight(p: A => Boolean): LazyList[A] =
    this.foldRight(LazyList.empty)((head, takeWhile_of_tail) =>
      if p(head) then LazyList.cons(head, takeWhile_of_tail)
      else LazyList.empty
    )

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((head, for_all_of_tail) =>
      p(head) && for_all_of_tail
    )

  def headOption: Option[A] = this match {
    case LazyList.Empty => None
    case LazyList.Cons(h, t) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
