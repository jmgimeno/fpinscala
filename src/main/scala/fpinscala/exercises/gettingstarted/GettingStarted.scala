package fpinscala.exercises.gettingstarted

import fpinscala.answers.streamingio.EffectfulPulls.Pull.done

// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  // Per a veure la màgia darrera de @main podeu
  // consultar https://docs.scala-lang.org/scala3/book/methods-main-methods.html

  // @main def printAbs: Unit =
  //   println(formatAbs(-42))

  // Si definim el main així (esborrant l'anterior)
  // el plugin ho detecta, pero el run | debug apareix
  // a sobre de l'objecte

  // def main(args: Array[String]): Unit =
  //   println("patata")

  // A definition of factorial, using a local, tail recursive function
  // - transparència referencial
  // - no usa pila (espai constant)
  def factorial(n: Int): Int =
    @annotation.tailrec
    def go(i: Int, acc: Int): Int = // n! * acc
      if i > 0 then go(i - 1, i * acc)
      else acc

    go(n, 1)

  // Another implementation of `factorial`, this time with a `while` loop
  // - no transparència referencial (de la implementació)
  // - no usa pila (espai constant)
  def factorialIter(n: Int): Int =
    var acc = 1
    var i = n
    while i > 0 do
      acc = acc * i
      i = i - 1
    acc

  // - transparència referencial
  // - usa pila (espai lineal => pot llençar StackOverflowError)
  def factorialRec(n: Int): Int =
    if n == 0 then 1
    else n * factorialRec(n - 1)

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int) = fibRec2(n)

  // 0 1 0+1=1 1+1=2 1+2=3 2+3=5 3+5=8 ...
  // f(0) = 0
  // f(1) = 1
  // f(2) = f(1) + f(0) = 1

  def fibIter(n: Int): Int =
    var current = 0
    var next = 1
    for _ <- 1 to n do
      val tmp = current
      current = next
      next = tmp + next
    current

  def fibRec(n: Int): Int =
    if n == 0 then 0
    else if n == 1 then 1
    else fibRec(n - 1) + fibRec(n - 2)

  def fibRecFinal(n: Int): Int =
    @annotation.tailrec
    def loop(i: Int, current: Int, next: Int): Int =
      if i == 0 then current
      else loop(i - 1, next, current + next)
    loop(n, 0, 1)

  // Transparència referencial
  // fibRecFinal(4)
  // loop(4, 0, 1)
  // loop(3, 1, 1)
  // loop(2, 1, 2)
  // loop(1, 2, 3)
  // loop(0, 3, 5)
  // 3

  // Transparència referencial
  // fibRec(4)
  // fibRec(3) + fibRec(2)
  // fibRec(2) + fibRec(1) + fibRec(2)
  // fibRec(1) + fibRec(0) + fibRec(1) + fibRec(2)
  // 1 + 0 + 1 + fibRec(2)
  // 2 + fibRec(1) + fibRec(0)
  // 2 + 1 + 0
  // 3

  // Another way to define the fibonacci, this time using a method
  // which returns a pair of values
  def fibRec2(n: Int) =
    def fibRecPair(n: Int): (Int, Int) =
      if n == 0 then (0, 1)
      else
        val (previous, current) = fibRecPair(n - 1)
        (current, previous + current)
    fibRecPair(n)(0)

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))

object FormatAbsAndFactorial:

  import MyProgram.*

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  @main def printAbsAndFactorial: Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

object TestFib:

  import MyProgram.*

  // test implementation of `fib`
  @main def printFib: Unit =
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println(
      "Actual:   %d, %d, %d, %d, %d, %d, %d".format(
        fib(0),
        fib(1),
        fib(2),
        fib(3),
        fib(4),
        fib(5),
        fib(6)
      )
    )

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions:

  import MyProgram.*

  // Some examples of anonymous functions:
  @main def printAnonymousFunctions: Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))

object MonomorphicLinearSearch:

  // First, a findFirst, specialized to `String`.
  // Ideally, we could generalize this to work for any `Array` type.
  def findFirst(ss: Array[String], key: String): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      // If `n` is past the end of the array, return `-1`
      // indicating the key doesn't exist in the array.
      if n >= ss.length then -1
      // `ss(n)` extracts the n'th element of the array `ss`.
      // If the element at `n` is equal to the key, return `n`
      // indicating that the element appears in the array at that index.
      else if ss(n) == key then n
      else loop(n + 1) // Otherwise increment `n` and keep looking.
    // Start the loop at the first element of the array.
    loop(0)

object PolymorphicFunctions:

  def findFirst[A](ss: Array[A], key: A): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      // If `n` is past the end of the array, return `-1`
      // indicating the key doesn't exist in the array.
      if n >= ss.length then -1
      // `ss(n)` extracts the n'th element of the array `ss`.
      // If the element at `n` is equal to the key, return `n`
      // indicating that the element appears in the array at that index.
      else if ss(n) == key then n
      else loop(n + 1) // Otherwise increment `n` and keep looking.
    // Start the loop at the first element of the array.
    loop(0)

  // Here's a polymorphic version of `findFirst`, parameterized on
  // a function for testing whether an `A` is the element we want to find.
  // Instead of hard-coding `String`, we take a type `A` as a parameter.
  // And instead of hard-coding an equality check for a given key,
  // we take a function with which to test each element of the array.
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      // If the function `p` matches the current element,
      // we've found a match and we return its index in the array.
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  /*
  val names = Array("pear", "apple, "potato", "banana")
  var pos = findFirst(names, "potato") // 2

  val pos2 = findFirst(name, n => n == "potato")

  findFirst(names, n => n == "potato")

  as = Array("pear", "apple, "potato", "banana")
  as.length = 4
  p  = n => n == "potato"

  loop(0)  // n == 0

  p(as(n))
  p(as(0))
  p("pear")
  "pear" == "potato"
  false

  loop(1)

  p(as(n))
  p(as(1))
  p("apple")
  "apple" == "potato"
  false

  loop(2)

  p(as(n))
  p(as(2))
  p("potato")
  "potato" == "potato"
  true

  2
   */

  /*
  val pos = findFirst(names, _.length > 25)
   */

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted increasingly
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(i: Int): Boolean =
      if i >= as.length - 1 then true
      else if gt(as(i), as(i + 1)) then false
      else loop(i + 1)
    loop(0)

  /*
  val sortedNamed = isSorted(names, _ >= _)
   */

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // Shit all the way down this hole
  def curry2[Object1, Object2, Object3](
      function1: (Object1, Object2) => Object3
  ): Object1 => (Object2 => Object3) =
    (o1: Object1) => (o2: Object2) => function1(o1, o2)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) =>
      val g: B => C = f(a)
      g(b)

  def uncurry2[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def uncurry3[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f.apply(a).apply(b)

  def uncurry4[A, B, C](f: A => (B => C)): (A, B) => C =
    f(_)(_)

  /*
  str.toUpperCase().charAt(0)

  var str2 = str.toUpperCase()
  str2.charAt(0)
   */

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
   */

  // Exercise 5: Implement `compose`

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  // Exercise 6: return the function which returns the function
  // than applies f n times on the A that is passed
  // If the function is never applies (n <= 0) then the given A
  // is returned

  def iterate[A](n: Int, f: A => A): A => A =
    if n <= 0 then (a: A) => a
    else compose(f, iterate(n - 1, f))

  def iterate2[A](n: Int, f: A => A): A => A =
    @annotation.tailrec
    def go(n: Int, acc: A => A): A => A =
      if n <= 0 then acc
      else go(n - 1, compose(f, acc))
    go(n, a => a)

  def iterate3[A](n: Int, f: A => A): A => A =
    if n <= 0 then a => a
    else
      val g = iterate3(n / 2, f)
      val h = compose(g, g)
      if n % 2 == 0 then h
      else compose(f, h)

  def iterate4[A](n: Int, f: A => A): A => A =
    (a: A) =>
      if n <= 0 then a
      else iterate4(n - 1, f)(f(a))

  def iterate5[A](n: Int, f: A => A): A => A =
    @annotation.tailrec
    def go(n: Int, acc: A): A =
      if n <= 0 then acc
      else go(n - 1, f(acc))
    a => go(n, a)

  @main def iterateExamples =
    def sum(a: Int, b: Int) =
      iterate(a, (n: Int) => n + 1)(b)

    def mult(a: Int, b: Int) =
      iterate2(b, (n: Int) => n + a)(0)

    def exp(a: Int, b: Int) =
      iterate3(b, (n: Int) => n * a)(1)

    def count(n: Int) =
      iterate4(n, (n: Int) => n + 1)(0)

    def exp2(n: Int) =
      iterate5(n, (n: Int) => 2 * n)(1)

    println(s"12 + 30 = ${sum(12, 30)}")
    println(s"12 * 30 = ${mult(12, 30)}")
    println(s"2^11 = ${exp(2, 11)}")
    println(s"42 = ${count(42)}")
    println(s"2^11 = ${exp2(11)}")
