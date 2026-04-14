import fpinscala.exercises.state.RNG

val rng = RNG.Simple(42)

val (n1, rng2) = rng.nextInt

rng.nextInt


val (n2, rng3) = rng2.nextInt

def randomPair1(rng: RNG): (Int,Int) =
  val (i1,rng2) = rng.nextInt
  val (i2,_) = rng2.nextInt
  (i1,i2)

randomPair1(rng3)
randomPair1(rng3)

RNG.ints(5)(rng3)(0).foreach(println)

RNG.ints2(5)(rng3)(0).foreach(println)
