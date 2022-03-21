package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] =
    this match
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Right(a) => f(a)
      case Left(e) => Left(e) // tipus Either[EE, B]
      // case l => l que no funciona
      // l: Either[E, A] però necessites és Either[EE, B]
      // EE >: E  es compleix que Either[EE, B] >: Either[E, B]
      // un Either[E, B] és un Either[EE, B]
      // Per construir un Either[E, B] hi ha dues possibilitats:
      //    - Right(b) a partir de b: B
      //    - Left(e) a partir d'un e: E

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(_) => b
      case r => r       // Either[EE, B] >: Either[E, A]

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => eb.map(b => f(a, b)))

  def map2_2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      a <- this
      b <- eb
    yield f(a, b)

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil):Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

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
    (a, b) match
      case (Left(a), Left(b)) => Left(a ++ b)
      case (Left(a), _) => Left(a)
      case (_, Left(b)) => Left(b)
      case (Right(a), Right(b)) => Right(f(a, b))

  def traverseAll[E, A, B](es: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    es.foldRight(Right(Nil):Either[List[E], List[B]])((a, acc) =>
      map2All(f(a), acc, _ :: _)
    )

  def sequenceAll[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(es, identity)
