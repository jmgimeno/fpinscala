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

  //                                  : RNG => (B, RNG)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

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

  //                                                           RNG => (C, RNG)
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

  /*
  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(Some(List.empty[A])) { (oa, acc) =>
      map2(oa, acc)(_ :: _)
   */
  def sequenceViaFoldRight[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A])) { (ra, ras) =>
      map2(ra, ras)(_ :: _)
    }

  def intsViaSequence[A](rand: Rand[A])(count: Int): Rand[List[A]] =
    sequence(List.fill(count)(rand))

  //                                              RNG => (B, RNG)
  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = ra(rng)
      val rb = f(a)
      rb(rng2)

  /*
  def nonNegativeLessThan(n: Int): Rand[Int] =
    map(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n-1) - mod >= 0
      then mod
      else nonNegativeLessThan(n)(???) <- impossible => we need another combinator
    }
   */

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0
      then unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r) { a =>
      // Rand[B]
      unit(f(a))
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    // underlying: State[S, A]: S => (A, S)
    //                     S => (B, S)
    def map[B](f: A => B): State[S, B] =
      (s: S) =>
        val (a, s2) = underlying(s)
        (f(a), s2)

    // underlying: State[S, A]: S => (A, S)
    //             sb: S => (B, S)
    //                                               S => (C, S)
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      (s: S) =>
        val (a, s2) = underlying(s)
        val (b, s3) = sb(s2)
        (f(a, b), s3)

    //                                   S => (B, S)
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      (s: S) =>
        val (a, s2) = underlying(s)
        f(a)(s2)

    def *>[B](andThen: State[S, B]): State[S, B] =
      flatMap(_ => andThen)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  // unit, sequence, traverse

  //                    S => (A, S)
  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs match
      case head :: next =>
        // head: State[S, A]
        // next: List[State[S, A]]
        // ???: State[S, List[A]]
        head.map2(sequence(next))(_ :: _)
      case Nil => unit(Nil: List[A])

  def traverse[S, A, B](rs: List[A])(f: A => State[S, B]): State[S, List[B]] =
    rs.foldRight(unit(Nil: List[B])) { (a, stateListB) =>
      f(a).map2(stateListB)(_ :: _)
    }

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, melonGums: Int, coins: Int)

/*
- Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
- Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
- Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
- A machine that’s out of candy ignores all inputs.
 */

object Candy:
  import Input.*
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      m <- State.get
    yield (m.melonGums, m.coins)

  def update(i: Input)(m: Machine): Machine =
    (i, m) match
      case (Coin, Machine(true, melonGums, coins)) if melonGums > 0 =>
        Machine(false, melonGums, coins + 1)
      case (Turn, Machine(false, melonGums, coins)) if melonGums > 0 =>
        Machine(true, melonGums - 1, coins)
      case _ => m
