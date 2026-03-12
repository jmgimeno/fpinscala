package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Option.None => default
    case Option.Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Option.None => None
    case Option.Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Option.None => ob
    case Option.Some(a) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Option.Some(a) if f(a) => this
    case _ => None
  }

  def filter_viaFlatMap(f: A => Boolean): Option[A] = {
    // A => Option[A]
    flatMap(a => if f(a) then Some(a) else None)
  }

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

  def variance_first_level(xs: Seq[Double]): Option[Double] = {
    mean(xs) match {
      case Option.None => None
      case Option.Some(m) =>
        val squaredDiffs = xs.map(x => math.pow(x - m, 2))
        mean(squaredDiffs)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2_patternMatching[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    (oa, ob) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap(a =>
      ob.map(b =>
        f(a, b)
      )
    )

  def map2_for[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- oa
      b <- ob
    yield f(a, b)

  def sequence[A](as: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = ???
