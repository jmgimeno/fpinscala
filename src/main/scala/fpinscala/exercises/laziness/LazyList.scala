package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def map[B](f: A => B): LazyList[B] = this match
    case LazyList.Empty => LazyList.empty
    case LazyList.Cons(h, t) => LazyList.cons(f(h()), t().map(f))

  def headOption: Option[A] = this match
    case LazyList.Empty => None
    case LazyList.Cons(h, _) => Some(h())

  def tailOption: Option[LazyList[A]] = this match
    case LazyList.Empty => None
    case LazyList.Cons(_, t) => Some(t())

  def tail: LazyList[A] = this match
    case LazyList.Cons(_, t) => t()
    case _ => sys.error("tail of empty list")

  def toList: List[A] = this match
    case LazyList.Empty => Nil
    case LazyList.Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes
  // its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

  def exists(p: A => Boolean): Boolean =
    // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)`
    // returns `true`, `b` will never be evaluated and the computation terminates early.
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case LazyList.Cons(h, t) if n > 0 =>
      LazyList.cons(h(), t().take(n - 1))
    case _ => LazyList.empty

  def drop(n: Int): LazyList[A] = this match
    case LazyList.Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case LazyList.Cons(h, t) if p(h()) =>
      LazyList.cons(h(), t().takeWhile(p))
    case _ => LazyList.empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true){ (a, forall_on_tail) => p(a) && forall_on_tail}

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def takeWhile_viaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty){ (a, takeWhile_on_tail) =>
      if p(a) then LazyList.cons(a, takeWhile_on_tail)
      else LazyList.empty
    }

  def headOption_viaFoldRight: Option[A] =
    foldRight(Option.empty) { (a, _) =>
      Some(a)
    }

  def map_viaFoldRight[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty) { (a, map_on_tail) =>
      LazyList.cons(f(a), map_on_tail)
    }

  def filter_viaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty) { (a, filter_on_tail) =>
      if p(a) then
        LazyList.cons(a, filter_on_tail)
      else
        filter_on_tail
    }

  def append_viaFoldRight[A2 >: A](that: LazyList[A2]): LazyList[A2] =
    this.foldRight(that) { (a, append_on_tail) =>
      LazyList.cons(a, append_on_tail)
    }

  def flatMap_viaFoldRight[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty) { (a, acc) =>
      f(a).append_viaFoldRight(acc)
    }

  def map_viaUnfold[B](f: A => B): LazyList[B] =
    LazyList.unfold(???) { state =>
      ???
    }

  def take_viaUnfold[B](n: Int): LazyList[B] =
    LazyList.unfold(???) { state =>
      ???
    }

  def takeWhile_viaUnfold[B](p: A => Boolean): LazyList[B] =
    LazyList.unfold(???) { state =>
      ???
    }

  def zipWith_viaUnfold[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    LazyList.unfold(???) { state =>
      ???
    }

  def zipAll_viaUnfold[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold(???) { state =>
      ???
    }

  def startsWith[B >: A](prefix: LazyList[B]): Boolean =
    zipAll_viaUnfold(prefix).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

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

  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    LazyList.cons(n, from(n + 1))

  def zipWith[A, B, C](as: LazyList[A], bs: LazyList[B])(f: (A, B) => C): LazyList[C] =
    (as, bs) match
      case (LazyList.Cons(ah, at), LazyList.Cons(bh, bt)) =>
        LazyList.cons(f(ah(), bh()), zipWith(at(), bt())(f))
      case _ => sys.error("only for infinite lazy lists")

  lazy val fibs: LazyList[Int] =
    LazyList.cons(0, LazyList.cons(1, zipWith(fibs, fibs.tail)(_ + _)))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => LazyList.empty
      case Some((a, nextState)) => LazyList.cons(a, unfold(nextState)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some(1, ()))


