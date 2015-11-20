package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  // Exercise 1
  def toList: List[A] = {
    val l = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(as: Stream[A]): List[A] = as match {
      case Empty => l.toList
      case Cons(h, t) => { l += h(); go(t()) }
    }
    go(this)
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0  => t().drop(n-1)
    case _ => this
    }

  // Exercise 3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5
  def takeWhile_fr(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, sa) => if (p(a)) cons(a, sa) else Empty)

  // Exercise 6
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, sb) => cons(f(a), sb))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, sa) => if (p(a)) cons(a, sa) else sa)

  def append[B >: A](sa: Stream[B]): Stream[B] =
    foldRight(sa)(cons(_, _))

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight[Stream[B]](empty)((a, s) => f(a).append(s))

  // Exercise 13
  def map_uf[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def take_uf(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
      case _ => None
    }

  def takeWhile_uf(f: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith_uf[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty       ) => Some((Some(ha()),       None), (ta(), empty))
      case (Empty,        Cons(hb, tb)) => Some((None,       Some(hb())), (empty, tb()))
      case _ => None
    }

  // Exercise 14
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  // Exercise 15
  def tails: Stream[Stream[A]] = sys.error("todo")

  // Exercise 16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 9
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Exercise 10

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = (as, bs) match {
    case (Cons(ha, ta), Cons(hb, tb)) => cons(f(ha(), hb()), zipWith(ta(), tb())(f))
    case _ => empty
  }

  val fibs_zw: Stream[Int] = cons[Int](0, cons(1, zipWith(fibs, fibs.drop(1))(_ + _)))

  // Exercise 11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }

  // Exercise 12
  def constant_uf[A](a: A) = unfold(())(_ => Some((a, ())))
  def from_uf(n: Int) = unfold(n)(i => Some(i, i+1))
  def fibs_uf = unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0+f1)) }
}