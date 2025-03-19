package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case LazyList.Empty => None
    case LazyList.Cons(h, _) => Some(h())

  def tailOption: Option[LazyList[A]] = this match
    case LazyList.Empty => None
    case LazyList.Cons(_, t) => Some(t())

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

  def forAll(p: A => Boolean): Boolean = ???

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
    else cons(as.head, apply(as.tail *))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = ???

  def from(n: Int): LazyList[Int] = ???

  lazy val fibs: LazyList[Int] = ???

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
