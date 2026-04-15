package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    (if n < 0 then -(n + 1) else n, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1.0), rng2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then (Nil, rng)
    else {
      val (head, rng2) = rng.nextInt
      val (tail, rng3) = ints(count - 1)(rng2)
      (head :: tail, rng3)
    }

  def ints_TR(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG)=
      if count <= 0 then (acc, rng)
      else {
        val (elem, rng2) = rng.nextInt
        go(count - 1, elem :: acc, rng2)
      }
    go(count, Nil, rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((a, b) => (a, b))

  val randIntDouble_v2: Rand[(Int, Double)] =
    both(int, RNG.double)

  val randDoubleInt_v2: Rand[(Double, Int)] =
    both(RNG.double, int)

  val randDoubleInt_v3: Rand[(Double, Int)] =
    map(randIntDouble_v2)(_.swap)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil)) { (ra: Rand[A], acc: Rand[List[A]]) =>
      map2(ra, acc)(_ :: _)
    }

  def ints_viaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(RNG.int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if i + (n - 1) - mod >= 0 then
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2)
  }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      val rb: Rand[B] = f(a)
      rb(rng2)

  def nonNegativeLessThan_viaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0
        then unit(mod)
        else nonNegativeLessThan_viaFlatMap(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

object Local:

  // I define a local object to hold the extension methods to
  // no interfere with the "normal" methods defined in the
  // outer object.

  extension[A](ra: RNG.Rand[A]) {
    def flatMap[B](f: A => RNG.Rand[B]): RNG.Rand[B] =
      RNG.flatMap(ra)(f)

    def map[B](f: A => B): RNG.Rand[B] =
      RNG.map(ra)(f)
  }

  def map2_viaFor[A,B,C](ra: RNG.Rand[A], rb: RNG.Rand[B])(f: (A, B) => C): RNG.Rand[C] =
    for
      a <- ra
      b <- rb
    yield f(a, b)

end Local

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
