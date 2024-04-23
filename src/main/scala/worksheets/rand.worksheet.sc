import fpinscala.exercises.state.RNG
import RNG.*

val rng = Simple(42L)

// int is the action for generating integers
int(rng)

val bool: Rand[Boolean] = map(int)(i => i % 2 == 0)

bool(rng)

enum Direction:
  case North, South, East, West

val direction: Rand[Direction] = map(int)(i => Direction.fromOrdinal(i % 4))

direction(rng)

val r1 = Simple(2024L)
val (n1, r2) = int(r1)
val (n2, r3) = int(r2)
val (n3, r4) = int(r3)
(List(n1, n2, n3), r4)

val int3 = List(int, int, int)
val seq3 = sequence(int3)
seq3(r1)

extension [A](r: Rand[A])
  def map[B](f: A => B): Rand[B] =
    RNG.map(r)(f)
  def flatMap[B](f: A => Rand[B]): Rand[B] =
    RNG.flatMap(r)(f)

val for1 =
  for
    d <- direction
    i <- int
    l <- seq3
  yield (d, i, l)

for1(rng)

// programació combinant coses amb for =
// combinar map + flatMap
// programació monàdicab 
def mapViaFor[A, B](r: Rand[A])(f: A => B): Rand[B] =
  for
    a <- r
  yield f(a)

def map2ViaFor[A, B, C](ra: Rand[A], rb: Rand[B])(
  f: (A, B) => C
): Rand[C] =
  for
    a <- ra
    b <- rb
  yield f(a, b)

def flatMapViaFor[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
  for 
    a <- r
    b <- f(a)
  yield b
  
