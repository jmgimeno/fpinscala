package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B = this match
    case None => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case None => None
    case Some(a) => f(a)

  def flatMap2[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None => ob
    case oa => oa // as Option is covariant and B>:A then Option[B]>:Option[A] so we can return oa

  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some).getOrElse(ob)

  // oa is a variable that is bound to the Some(a) value
  // and we can use it in the right-hand side to refer to
  // the whole expression and not create a new value
  def filter(f: A => Boolean): Option[A] = this match
    case oa@Some(a) if f(a) => oa
    case _ => None

  def filter2(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

  def filter3(f: A => Boolean): Option[A] =
    map(a => if f(a) then Some(a) else None).getOrElse(None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(mu => mean(xs.map(x => (x - mu) * (x - mu))))

  def variance_2(xs: Seq[Double]): Option[Double] =
    mean(xs.map(x => x * x))
      .flatMap(mux2 => mean(xs).map(mu => mux2 - mu * mu))

  // for-expressions will be presented later in the course
  // but this is an interesting use-case for them
  def variance_3(xs: Seq[Double]): Option[Double] = for {
    mux2 <- mean(xs.map(x => x * x))
    mu <- mean(xs)
  } yield mux2 - mu * mu

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    (oa, ob) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None

  def sequence_2[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case h :: t => map2(h, sequence_2(t))(_ :: _)

  // sequence follows the foldRight pattern
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil):Option[List[A]])((a, acc) => map2(a, acc)(_ :: _))

  def sequence_4[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case h :: t =>
        for
          a <- h
          as <- sequence_4(t)
        yield a :: as

  def sequence_5[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil):Option[List[A]]) {
      (h, acc) => for {
        a <- h
        as <- acc
      } yield a :: as
    }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil):Option[List[B]])((h, acc) => map2(f(h), acc)(_ :: _))

  // if we compare the solutions of sequence_2 and traverse we see that the
  // only difference is the h <-> f(h) and we want them to be equal, so the
  // funcion f to use is h => h (a.k.a. identity)
  def sequence_3[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(h => h)
