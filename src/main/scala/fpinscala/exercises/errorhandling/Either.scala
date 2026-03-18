package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter

import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Either.Left(e) => Left(e)
      case Either.Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Either.Left(e) => Left(e)
      case Either.Right(a) => f(a)
    }

  def orElse[EE, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Either.Left(_) => b
      case Either.Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](eeb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- eeb
    yield f(a, b)

object Either:
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil) /*Either[E, List[B]]*/) {
      (a: A, acc: Either[E, List[B]]) =>
        f(a).map2(acc)(_ :: _) /*Either[E, List[B]]*/
    }

  def sequence[EE, AA](es: List[Either[EE, AA]]): Either[EE, List[AA]] = {
    /*
    es: List[Either[EE,AA]] <-> as: List[A]
      -> Either[EE,AA] = A
    f: A => Either[E, B]
      -> f: Either[EE,AA] => Either[E, B]
    resultat sequence: Either[EE,List[AA]]
    resultat traverse: Either[E, List[B]]
      -> E <-> EE
      -> AA <-> B
      -> f: Either[EE,AA] => Either[E, B]
        -> f: Either[EE,AA] => Either[EE, AA]
     */
    traverse(es)(a => a) // traverse(es)(identity)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a, b) match {
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e1), _) => Left(e1)
      case (_, Left(e2)) => Left(e2)
      case (Right(a), Right(b)) => Right(f(a, b))
    }

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight(Right(Nil)) { (a, acc) =>
      map2All(f(a), acc, _ :: _)
    }

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)
