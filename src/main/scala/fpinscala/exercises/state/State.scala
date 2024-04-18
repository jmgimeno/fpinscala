package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
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

  // -2^31 ..0.. 2^31-1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (valueInt, nextRNG) = rng.nextInt
    (if valueInt >= 0 then valueInt else -(valueInt + 1), nextRNG)

  /*
  -2^31 -2^31+1 -2^31+2 ..... -2 -1 0 +1 +2 ...... 2^31-3 2^31-2 2^31-1

  2^31-1 2^31-2  2^31-3       +1  0
   */

  // generar double [0, 1)
  def double(rng: RNG): (Double, RNG) =
    val (valueInt, nextRng) = nonNegativeInt(rng)
    val valueDouble: Double = valueInt / (Int.MaxValue.toDouble + 1.0)
    (valueDouble, nextRng)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (valueInt, nextRng) = rng.nextInt
    val (valueDouble, nextRng2) = double(nextRng)
    ((valueInt, valueDouble), nextRng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (valueInt, nextRng) = rng.nextInt
    val (valueDouble, nextRng2) = double(nextRng)
    ((valueDouble, valueInt), nextRng2)

  def doubleInt2(rng: RNG): ((Double, Int), RNG) =
    val ((valueInt, valueDouble), rngNext) = intDouble(rng)
    ((valueDouble, valueInt), rngNext)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (valueDouble, nextRng) = double(rng)
    val (valueDouble2, nextRng2) = double(nextRng)
    val (valueDouble3, nextRng3) = double(nextRng2)
    ((valueDouble, valueDouble2, valueDouble3), nextRng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @annotation.tailrec
    def go(count: Int, currentRng: RNG, l: List[Int]): (List[Int], RNG) =
      if count > 0 then
        val (valInt, nextRng) = currentRng.nextInt
        go(count - 1, nextRng, valInt :: l)
      else (l, currentRng)
    go(count, rng, Nil)

  type Rand[+A] = RNG => (A, RNG)

  // int is an action which generates a random integer
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def map_alt[A, B](s: Rand[A])(f: A => B)(rng: RNG): (B, RNG) =
    val (a, rng2) = s(rng)
    (f(a), rng2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = ???

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

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get[S]
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
