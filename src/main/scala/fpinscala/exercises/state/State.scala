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

  extension [A](ra: Rand[A]) {
    // We define them here to have map / flatMap
    // defined as methods on the type and be able to
    // use for notation
    def map[B](f: A => B): Rand[B] =
      rng =>
        val (a, rng2) = ra(rng)
        (f(a), rng2)

    def flatMap[B](f: A => Rand[B]): Rand[B] =
      rng =>
        val (a, rng2) = ra(rng)
        f(a)(rng2)
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    (if n < 0 then -(n + 1) else n, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  // I do a reverse to have this property: the first generated element
  // is the first element in the generated list
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def go(i: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if i <= 0 then (acc.reverse, rng)
      else
        val (n, rng2) = rng.nextInt
        go(i - 1, n :: acc, rng2)

    go(count, Nil, rng)

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    if count <= 0 then (Nil, rng)
    else
      val (n, rng2) = rng.nextInt
      val (ns, rng3) = ints2(count - 1)(rng2)
      (n :: ns, rng3)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    //   Rand[List[A]]    Rand[A]    Rand[List[A]]
    rs.foldRight(unit(Nil: List[A])) { (ra, sequence_of_tail) =>
      map2(ra, sequence_of_tail)(_ :: _)
      //      rng => {
      //        val (a, rng2) = ra(rng)
      //        val (as, rng3) = sequence_of_tail(rng2)
      //        (a :: as, rng3)
      //      }
    }

  def ints_viaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  //                                 RNG => (List[Int], RNG)
  def ints_viaSequence2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r) { a =>
      unit(f(a))
    }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    for {
      a <- ra
      b <- rb
    } yield f(a, b)

// State = State ACTION !!!!
opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    //                     S => (B, S)
    def map[B](f: A => B): State[S, B] =
      s => {
        val (a, s2) = underlying(s)
        (f(a), s2)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- underlying
        b <- sb
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s2) = underlying(s)
        f(a)(s2)
      }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def traverse[S, A, B](sas: List[A])(f: A => State[S, B]): State[S, List[B]] =
    sas.foldRight(unit[S, List[B]](Nil)) { (head, traverse_of_tail) =>
      f(head).map2(traverse_of_tail)(_ :: _)
    }

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    traverse(sas)(identity)

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  /*
  - Inserting a coin into a locked machine will cause it to unlock
  if there’s any candy left.
  - Turning the knob on an unlocked machine will cause it to dispense
  candy and become locked.
  - Turning the knob on a locked machine or inserting a coin into an
  unlocked machine does nothing.
  - A machine that’s out of candy ignores all inputs.
  */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    // Hint: Each input represents a transition function that transforms
    // the machine. In a OO setup we'd have a method on machine of type
    // void input(i: Input) that would mutate the inner state of the
    // Machine object. Here we have a transformation function.
    def update(i: Input)(m: Machine): Machine =
      (i, m) match
        case (_, Machine(_, 0, _)) => m
        case (Input.Coin, Machine(true, candies, coins)) =>
          Machine(false, candies, coins + 1)
        case (Input.Turn, Machine(false, candies, coins)) =>
          Machine(true, candies - 1, coins)
        case (Input.Coin, Machine(false, _, _)) => m
        case (Input.Turn, Machine(true, _, _)) => m

    for {
      _ <- State.traverse(inputs)(input => State.modify(update(input)))
      m <- State.get
    } yield (m.coins, m.candies)
