package fpinscala.exercises.state

import scala.annotation.tailrec

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      println("Generació")
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (current, nextRng) = rng.nextInt
    (if current >= 0 then current else -(current + 1), nextRng)

  // 0 <= double < 1
  def double(rng: RNG): (Double, RNG) =
    val (pos, rng2) = nonNegativeInt(rng)
    (pos / (Int.MaxValue.toDouble + 1.0), rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)

  // To generate the random elements in the same order as in the
  // figure on slide 5
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count == 0
    then (List.empty, rng)
    else
      val (i, rng2) = int(rng)
      val (is, rng3) = ints(count - 1)(rng2)
      (i :: is, rng3)

  def intsTR(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(count: Int, is: List[Int], rng: RNG): (List[Int], RNG) =
      if count == 0 then (is, rng)
      else
        val (i, rng2) = rng.nextInt
        go(count - 1, i :: is, rng2)
    val (l, rngEnd) = go(count, List.empty, rng)
    (l.reverse, rngEnd)

    // RNG => (C, RNG)
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  //                                      RNG => (List[A], RNG)
  def list[A](rand: Rand[A])(count: Int): Rand[List[A]] =
    println("Configuració")
    if count == 0
    then unit(List.empty)
    else map2(rand, list(rand)(count - 1))(_ :: _)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    // Rand[Int] Rand[Double]
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs match
      // head: Rand[A]
      // next: List[Rand[A]]
      // sequence(next): Rand[List[A]]
      // ???: Rand[List[A]]
      case head :: next => map2(head, sequence(next))(_ :: _)
      case Nil          => unit(Nil: List[A])

  def sequenceViaFoldRight[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A])) { (ra, ras) =>
      map2(ra, ras)(_ :: _)
    }

  def intsViaSequence[A](rand: Rand[A])(count: Int): Rand[List[A]] =
    sequence(List.fill(count)(rand))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = ???

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = ???

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = ???

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      ???

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      ???

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      ???

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
