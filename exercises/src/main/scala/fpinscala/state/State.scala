package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

 // Exercise 1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, next) = rng.nextInt
    if (int < 0) (-(int+1), next) else (int, next)
  }

  // Exercise 2
  def double(rng: RNG): (Double, RNG) = {
    val (num, rng2) = nonNegativeInt(rng)
    if (num != Int.MaxValue) (num.toDouble/Int.MaxValue, rng2)
    else (0.0, rng2)
  }

  // Exercise 3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (dbl, rng3) = double(rng2)
    ((int, dbl), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, dbl), rng2) = intDouble(rng)
    ((dbl, int), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dbl1, rng2) = double(rng)
    val (dbl2, rng3) = double(rng2)
    val (dbl3, rng4) = double(rng3)
    ((dbl1, dbl2, dbl3), rng4)
  }

  // Exercise 4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (Nil, rng)
    else {
      val (is, rng2) = ints(count-1)(rng)
      val (i,  rng3) = rng2.nextInt
      (i :: is, rng3)
    }
  }

  // -----------------------------------------------------

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5
  def double_m: Rand[Double] = {
    map(nonNegativeInt) { i =>
      if (i != Int.MaxValue) i.toDouble/Int.MaxValue
      else 0.0
    }
  }

  // Exercise 6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)
  
  // Exercise 7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs match {
      case Nil  => (Nil, rng)
      case h::t => {
        val (a,  rng2) = h(rng)
        val (as, rng3) = sequence(t)(rng2)
        (a::as, rng3)
      }
  }

  // Exercise 8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod > 0) unit(mod) else nonNegativeLessThan(n)
    }

  // Exercise 9
  def map_fm[A,B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(f andThen unit)

  def map2_fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S,+A](run: S => (A, S)) {

  // Exercise 10
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (a, next) = run(s)
      (f(a), next)
    }

  def map_fm[B](f: A => B): State[S, B] = flatMap(f andThen State.unit)

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, next) = run(s)
      val (b, nextNext) = sb.run(next)
      (f(a, b), nextNext)
    }

  def map2_for[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, next) = run(s)
      f(a).run(next)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // Exercise 10
  def unit[S, A](a: A): State[S, A] =
    State { s =>
      (a, s)
    }

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss match {
      case Nil  => unit(Nil)
      case h::t => for {
        a  <- h
        as <- sequence(t)
      } yield a::as
    }

  def lift2[S, A, B, C](f: (A, B) => C)(sa: State[S, A], sb:State[S, B]): State[S, C] =
    sa.map2(sb)(f)

  def sequence_fr[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight[State[S, List[A]]](unit(Nil))(lift2(_ :: _))

  // Exercise 11
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  type MachineState = State[Machine, (Int, Int)]

  def coin: MachineState =
    for {
      m <- get
      _ <- set {
        if (m.locked && m.candies > 0)
          Machine(locked = false, m.candies, m.coins+1)
        else
          m
      }
      nm <- get
    } yield (nm.candies, nm.coins)

  def turn: MachineState =
    for {
      m <- get
      _ <- set {
        if (!m.locked && m.candies > 0)
          Machine(locked = true, m.candies-1, m.coins)
        else
          m
      }
      nm <- get
    } yield (nm.candies, nm.coins)

  def inputToState(input: Input): MachineState = input match {
    case Coin => coin
    case Turn => turn
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(inputToState)).map(_.last)

}
