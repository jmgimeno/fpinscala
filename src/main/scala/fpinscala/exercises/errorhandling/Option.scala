package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Option.Some(get) => Some(f(get))
    case Option.None => None

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Option.Some(get) => f(get)
    case Option.None => None

  // NOTE:
  // def getOrElse(default: => A): A = ???
  // Covariant type A occurs in contravariant position in type A of value default
  // Why?

  // NOTE:
  // => means call-by-name (instead of the 'normal' call-by-value)
  def getOrElse[B >: A](default: =>
  B): B = this match
    case Option.Some(get) => get
    case Option.None => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case Option.Some(get) => this
    case Option.None => ob

  def filter(f: A => Boolean): Option[A] = this match
    case Option.Some(get) if f(get) => this
    case _ => Option.None

object Option:

  def failingFn(i: Int): Int =
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right
    // hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try
      val x = 42 + 5
      x + y
      // A `catch` block is just a pattern matching block like the ones we've seen.
      // `case e: Exception` is a pattern that matches any `Exception`, and it binds this
      // value to the identifier `e`. The match returns the value 43.
    catch case e: Exception => 43

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) match
      case Option.None => Option.None
      case Option.Some(mu) =>
        val squaredDiff = xs.map(x => (x - mu) * (x - mu))
        mean(squaredDiff)

  def variance_2(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { mu =>
      val squaredDiff = xs.map(x => (x - mu) * (x - mu))
      mean(squaredDiff)
    }

  def variance_3(xs: Seq[Double]): Option[Double] = ???

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    (oa, ob) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None

  def map2_2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap { a =>
      ob.map { b =>
        f(a, b)
      }
    }

  def map2_3[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    // for-comprehension
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  // * Programació imperativa
  // val a = oa
  // val b = ob
  // f(a, b)

  // define lift2

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil)) { (oa: Option[A], oas: Option[List[A]]) =>
      map2(oa, oas)(_ :: _)
    }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???
