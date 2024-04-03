package fpinscala.exercises.laziness

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def toList: List[A] =
    this match
      case Empty            => Nil
      case Cons(head, tail) => head() :: tail().toList

  // The book only defines the second argument as passed by name
  def foldRight[B](
      z: => B
  )(
      f: (=> A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    println("foldRight")
    this match
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  // retorna LazyList[A] amb els n primers elements de la llista i si acaba empty
  def take(n: Int): LazyList[A] =
    println(s"take $n")
    this match
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _                   => empty

  // retorna LazyList[A] eliminant els n elements (si els te) per inici o empty
  def drop(n: Int): LazyList[A] =
    println(s"drop $n")
    this match
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this

  // retorna LazyList[A] mentres es compleixi la propietat
  def takeWhile(p: A => Boolean): LazyList[A] =
    println("take while")
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty

  def takeWhile_viaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty) { (a, b) =>
      println("inside takeWhile lambda")
      if p(a) then cons(a, b) else empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b
    }

  def headOption: Option[A] =
    this match
      case Empty => None
      case Cons(h, t) => Some(h())

  def headOption_viaFoldRight: Option[A] = ???

  def tailOption: Option[LazyList[A]] =
    this match
      case Empty => None
      case Cons(h, t) => Some(t())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = ???

  def filter(p: A => Boolean): LazyList[A] = ???

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = ???

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = ???

  def startsWith[B](s: LazyList[B]): Boolean = ???

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  // apply creates a LazyList but it's eager !!!
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
