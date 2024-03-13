package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case None    => None
      case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B =
    this match
      case None    => default
      case Some(a) => a

  def flatMapPM[B](f: A => Option[B]): Option[B] =
    this match
      case None    => None
      case Some(a) => f(a)

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)
    // this: Option[A]
    // f: A => Option[B]
    // this.map(f): Option[Option[B]]
    // this.map(f).getOrElse(???): Option[B]

  def orElsePM[B >: A](ob: => Option[B]): Option[B] =
    this match
      case None => ob
      case _    => this
      // this: Option[A]

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch
      case e: Exception =>
        43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { mu =>
      mean {
        xs.map { x =>
          math.pow(x - mu, 2)
        }
      }
    }

  def varianceJava(xs: Seq[Double]): Double =
    variance(xs).getOrElse(throw new Exception("patata"))

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap { a =>
      ob.map { b =>
        f(a, b)
      }
    }

  def map2For[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- oa
      b <- ob
    yield f(a, b)
    
  /*
    val a = extract(oa)
    val b = extract(ob)
    f(a, b)
   */
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(Nil)) { (oa: Option[A], oas: Option[List[A]]) =>
      map2(oa,oas)((a: A, as: List[A]) => a :: as)
    }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(Some(Nil)) { (a: A, obs: Option[List[B]]) =>
      map2(f(a), obs)((b: B, bs: List[B]) => b :: bs)
    }
