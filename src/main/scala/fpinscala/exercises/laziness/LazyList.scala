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
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if f(h()) then Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] =
    // println(s"Take of $n")
    this match {
      case LazyList.Cons(h, _) if n == 1 => LazyList.cons(h(), LazyList.empty)
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

  def tailOption: Option[LazyList[A]] =
    this match {
      case LazyList.Empty => None
      case LazyList.Cons(h, t) => Some(t())
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def headOption_foldRight: Option[A] =
    foldRight(None)((head, _) => Some(head))

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty) { (head, map_of_tail) =>
      LazyList.cons(f(head), map_of_tail)
    }

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty) { (head, filter_of_tail) =>
      if p(head) then LazyList.cons(head, filter_of_tail)
      else filter_of_tail
    }

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that) { (head, append_of_tail) =>
      LazyList.cons(head, append_of_tail)
    }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty) { (head, flatMap_of_tail) =>
      f(head).append(flatMap_of_tail)
    }

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail *))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = {
    // LazyList.cons(a, continually(a))
    lazy val as: LazyList[A] = LazyList.cons(a, as)
    as
  }

  def from(n: Int): LazyList[Int] =
    LazyList.cons(n, from(n + 1))

  def map2[A, B, C](as: LazyList[A], bs: LazyList[B])(f: (A, B) => C): LazyList[C] =
    (as, bs) match {
      case (LazyList.Cons(ha, ta), LazyList.Cons(hb, tb)) =>
        LazyList.cons(f(ha(), hb()), map2(ta(), tb())(f))
      case _ => LazyList.empty
    }

  val fibs: LazyList[Int] =
    LazyList.cons(0,
      LazyList.cons(1,
        map2(fibs, fibs.drop(1))(_ + _)
      )
    )

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).fold(LazyList.empty) { (a, nextState) =>
      LazyList.cons(a, unfold(nextState)(f))
    }

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) {
      case (current, next) =>
        Some((current, (next, current + next)))
    }

  //  lazy val fibsViaUnfold: LazyList[Int] =
  //    unfold((0, 1)) {
  //      state =>
  //        Some((state(0), (state(1), state(0) + state(1))))
  //    }

  // Fixeu-vos que si en comptes de fer un unfold per generar una LazyList
  // amb els fibs fas un bucle per imprimir-los, les dues variables que uses
  // són les que es corresponen amb l'estat.
  // Aquest codi barreja generació i escriptura (viola el principi de responsabilitat
  // única)
  def fibsImperative(n: Int): Unit = {
    var current = 0
    var next = 1
    for _ <- 1 to n do {
      println(current)
      val tmp = current
      current = next
      next = tmp + next
    }
  }

  // Una altra manera de fer una cosa semblant seria implementant un iterador
  // (fixeu-vos que no es referencialment transparent ja que cada vegada que cridem
  // a hasNext/next obtenim un valor diferent). En canvi la LazyList és referencialment
  // transparent.
  // Les variables d'instància de l'iterador són l'estat de l'unfold.
  class FibIterator extends java.util.Iterator[Int] {
    var current = 0
    var next_ = 1

    override def hasNext: Boolean = true

    override def next(): Int =
      val result = current
      current = next_
      next_ = result + next_
      result
  }

  // Consumim l'iterador, imprimint cada valor
  // Responsabilitats separades:
  //   generació -> iterador
  //   bucle -> escriptura
  // NOTA: Aquesta mena de separació és la que aconseguim també amb
  // les lazyList.
  // Podriem fer:
  //     fibs.take(10).forEach(println)
  // (si tinguèssim l'operador forEach).
  def fibsIterator(n: Int): Unit = {
    val it = FibIterator()
    for _ <- 1 to n do {
      println(it.next())
    }
  }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n) { current =>
      Some((current, current + 1))
    }

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(()) { _ =>
      Some((a, ()))
    }

  val onesViaUnfold: LazyList[Int] =
    unfold(()) { _ =>
      Some((1, ()))
    }
