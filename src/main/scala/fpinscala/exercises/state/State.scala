package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG :
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
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, rng2) = rng.nextInt
    val r = if i < 0 then -(i + 1) else i
    (r, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (i, rng2) = nonNegativeInt(rng)
    val f = i / (Int.MaxValue.toDouble + 1)
    (f, rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng2) = rng.nextInt
    val (h, rng3) = double(rng2)
    ((i, h), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (h, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt

    ((h, i), rng3)

  def doubleInt_2(rng: RNG): ((Double, Int), RNG) =
    val ((i, h), rng2) = intDouble(rng)
    ((h, i), rng2)

  def doubleInt_3(rng: RNG): ((Double, Int), RNG) =
    val iD = intDouble(rng)
    ((iD._1._2, iD._1._1), iD._2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if count > 0 then
      val (i, rng2) = rng.nextInt
      val (list, rng3) = ints(count - 1)(rng2)
      (i :: list, rng3)
    else (Nil, rng)

  def ints_2(count: Int)(rng: RNG): (List[Int], RNG) =
    // The order of generation of the elements in the list
    // is the reverse of ints
    @annotation.tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) =
      if count == 0 then (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(count - 1, rng2, i :: acc)
      }
    go(count, rng, Nil)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    println("temps de map2")
    rng =>
      println("temps de generació")
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  /*
    Let's try to use foldRight which is the natural way to construct things on lists (respecting the order)

                                rs.foldRight(???1) ((ra, acc) => ???2))

    ???1:
      - Has the type of the result, that is, Rand[List[A]] = RNG => (List[A], RNG)
      - It's the value to return when rs list is empty, that is, we have no Rand[A]
      - If we have no means of creating A's the only List[A] we can generate is Nil
      - So we need a function: RNG => (Nil, RNG)
      - This can be obtained by unit(Nil)
     ???2:
      - Has the type of the result, that is, Rand[List[A]] = RNG => (List[A], RNG)
      - We can use the parameters a and acc which have type:
        ra: has the type of the elements of the list, that is, Rand[A]
        acc: is the result of sequence on the rest of the list, that is, has type Rand[List[A]]
      - So I have a Rand[A] and a Rand[List[A]] and I have to create a Rand[List[A]], what can I use?
        - map2 using a combining function (f) that given an A and a List[A] adds A to the List[A]
        - that is the equivalent of Cons in the scala library or _ :: _

    If yoy have problems with what is going on inside the foldRight, you can instead of using map2
    use its definition, that is:

    rs.foldRight(unit(Nil):Rand[List[A]])((ra, acc) =>
      rng =>                       // given a RNG
        val (a, rng2) = ra(rng)    // we extract an A and a new RNG from the Rand[A] in the List
        val (as, rng3) = acc(rng2) // we use the returned RNG to extract the rest of the List and the next RNG
        (a :: as, rng3)            // we add the element to the list and return it paired with the next RNG
    )

    But all of this minutiae is boilerplate that can be delegated to map2 and consider only in the way to combine
    an A and a List[A] which we have "inside" the structure of type Rand[A] and Rand[List[A]].

    The creation of the function and the weaving of the RNG is programmed once in map2 and we can concentrate in the
    "essential" task, which is the _ :: _

  */
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil):Rand[List[A]])((ra, acc) => map2(ra, acc)(_ :: _))

  def ints_3(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

// Outside the scope of State (this compilation unit State.scala) nobody knows that
// State[S, A] is just a synonym for S => (A, S)
// Inside the companion object I can use freely an State[S, A] as a S => (A, S)
// Outside the compilation unit the only thiog I can do on an State is use the methods
// defined in its interface that, in this case, are the extension methods.
// It's the same idea of the private part of a class: inside the class I knoe its internal
// representations, outside the class, I only know its interface.
opaque type State[S, +A] = S => (A, S)

object State:                  // S => (A, S)
  extension[S, A] (underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)
                           // S => (B, S)
    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, s2) = underlying(s)
        (f(a), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s =>
        val (a, s2) = underlying(s)
        val (b, s3) = sb(s2)
        (f(a, b), s3)

    def map2ViaFlatMap[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      underlying.flatMap(a => sb.map(b => f(a,b)))

    def map2ViaFlatMapFor[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = underlying(s)
        f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit(Nil):State[S, List[A]])((sa , acc) => sa.map2(acc)(_ :: _))

  def sequenceViaTraverse[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    traverse(actions)(identity)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit(Nil):State[S, List[B]])((a , acc) => f(a).map2(acc)(_ :: _))

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)).map(_ => ()))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:

  import Input.*

  def step(input: Input)(machine : Machine): Machine =
    (input, machine) match
      case (Coin, Machine(true, candies, coins)) if candies > 0
        => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins))
        => Machine(true, candies - 1, coins)
      case (_, machine) => machine

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      //_ <- State.sequence(inputs.map(i => State.modify(step(i))))
      _ <- State.traverse(inputs)(i => State.modify(step(i)))
      m <- State.get
    yield (m.coins, m.candies)

    /* Very ugly initial version (w/o thinking so much)
    State( machine =>
      val combined: Machine => Machine
        = inputs.foldRight(identity[Machine])
                          ((input, acc) => step(input).andThen(acc))
      val machine2 = combined(machine)
      ((machine2.coins, machine2.candies), machine2)
    )
    */

