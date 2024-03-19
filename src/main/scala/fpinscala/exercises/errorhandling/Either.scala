package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Right(a) => f(a)
      case Left(e)  => Left(e)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Right(a) => Right(a)
      case Left(_)  => b

  def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _)         => Left(e)
      case (_, Left(ee))        => Left(ee)

  def map2_for[EE >: E, B, C](
      eb: => Either[EE, B]
  )(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- eb
    yield f(a, b)

object Either:
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]]) {
      (ea: Either[E, A], acc: Either[E, List[A]]) =>
        ea.map2(acc)((a: A, as: List[A]) => a :: as): Either[E, List[A]]
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) =>
      f(a).map2(acc)(_ :: _)
    )

  def sequence_viaTraverse[E1, A1](
      es: List[Either[E1, A1]]
  ): Either[E1, List[A1]] =
    // A <==> Either[E1, A1]
    // B <==> A1
    // E <==> E1
    // f: A              => Either[E, B]  <==>
    //    Either[E1, A1] => Either[E, B]  <==>
    //    Either[E1, A1] => Either[E, A1] <==>
    //    Either[E1, A1] => Either[E1, A1]
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_))   => Left(es)
      case (Right(_), Left(es))   => Left(es)
      case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    as.foldRight(Right(Nil): Either[List[E], List[B]])((a, acc) =>
      map2All(f(a), acc, _ :: _)
    )

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] =
    traverseAll(as, identity)

  def map2AllGeneral[X, A, B, C](
      a: Either[X, A],
      b: Either[X, B],
      f: (A, B) => C,
      combineErrors: (X, X) => X
  ): Either[X, C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(es), Right(_))   => Left(es)
      case (Right(_), Left(es))   => Left(es)
      case (Left(es1), Left(es2)) => Left(combineErrors(es1, es2))

  def map2All_Alt[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] =
    map2AllGeneral(a, b, f, _ ++ _)
