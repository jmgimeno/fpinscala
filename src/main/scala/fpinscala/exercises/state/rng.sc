import fpinscala.exercises.state.*
import RNG.*

val rng = Simple(-908)

// nextInt is referentially transparent
// if we call it twice we get the same result
rng.nextInt
rng.nextInt

// int is like nextInt but with type Rand[Int] = RNG => (Int, RNG)
int(rng)
int(rng)

// We can call other Rand-type functions
nonNegativeInt(rng)
double(rng)

// To show that ints_2 generate the same elements in the reverse order
ints(4)(rng)
ints_2(4)(rng)

// double and int hve type Rand[Double] and Rand[Int]
val d: Rand[Double] = double
val i: Rand[Int] = int

// I can use map2 to create a Rand[(Double, Int)]
val r: Rand[(Double, Int)] = map2(d, i)((a, b) => (a, b))

// r is a RandRand[(Double, Int)], that is, a function with type
// RNG => ((Double, Int), RNG)
// We need to call it with and RNG to get the pair (and the new RNG)
val (p, rng2) = r(rng)
val (q, rng3) = r(rng2)
((p, q), rng3)

// We can use map2 to create more complex Rand's
println("abans de map2")
val rr = map2(r, r)((_, _))
println("després de map2")
println("abans de generar")
rr(rng)
println("després de generar")

// From a list of 10 Rand's that each one generates an Int
val l = List.fill(10)(nonNegativeInt)
// Sequence combines the list in a single Rand than generates a list of 10 ints
val s = sequence(l)
// The list is generates when I call the Rand passing an RNG
s(rng)

// Can use the same idea to generate lists od Doubles
val ld = List.fill(10)(double)
val (ds, rng4) = sequence(ld)(rng)
sequence(ld)(rng4)

// Can combina different generators (in this case scala infers the type AnyVal
// for the type of the elements of a list of a Double and an Int)
val lp = List(nonNegativeInt, double)
sequence(lp)(rng)

// Sequence in essence is not much different from ffff that is a function which
// returns a function.
def ffff(a: Int): Int => Int =
  (b: Int) => a * b
// When we call ffff we get back a function
val gggg = ffff(23)
// Then we can call this new function to get the final result
gggg(2)

ints_3(4)(rng)

