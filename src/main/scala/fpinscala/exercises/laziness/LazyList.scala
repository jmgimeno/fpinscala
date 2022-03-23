package fpinscala.exercises.laziness

// We import the definitions in the companion object
// so we can use cons, empty, etc.
import LazyList.*

enum LazyList[+A]:
  // If we define the value constructors Cons & Empty as private we can force
  // users of LazyList to only use the smart versions cons & empty
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // Simple recursive solution
  def toList: List[A] = this match
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => empty

  // Forces the evaluation of the tail when it's not needed
  def bad_take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty

  /*
  The idea is not to generate the call no t() when we
  know there is no need (when n == 1)

  Cons(() => h1, () => Cons(() => h2, () => Cons(() => h3, () => Empty))).take(1)
       --------  -------------------------------------------------------
          h                               t

  cpns(h(), t().take(0))
  cons(h1, Cons(() => h2, () => Cons(() => h3, () => Empty)).take(0))
  cons(h1, empty)

  */

  // Comentar que si la fem final la podem marcar com tailrec
  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  // It seems that h() is evaluated twice but if we only use cons
  // its "magic" of catches the value of h() so the second evaluation
  // only gets the catch value
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty

  def forAll(p: A => Boolean): Boolean = this match
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def forAll_2(p: A => Boolean): Boolean =
    this.foldRight(true)((a, acc) => p(a) && acc)

  def takeWhile_2(p: A => Boolean): LazyList[A] = ???

  def headOption_2: Option[A] = ???

  def map[B](f: A => B): LazyList[B] = ???

  def filter(p: A => Boolean): LazyList[A] = ???

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = ???

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = ???

  // This is to solve an example from datastructures we have not worked on
  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  // Bad cons that does not catch the evaluation of an already forced part
  def bad_cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    Cons(() => hd, () => tl)

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
