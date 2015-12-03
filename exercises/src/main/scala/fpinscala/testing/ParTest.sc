import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.testing.Gen
import fpinscala.testing.Prop
import fpinscala.testing.Prop.{Falsified, Proved}

val ES: ExecutorService = Executors.newCachedThreadPool

val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
  Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
)

Prop.run(p1)
val p2 = Prop.check {
  val p = Par.map(Par.unit(1))(_ + 1)
  val p2 = Par.unit(2)
  p(ES).get == p2(ES).get
}

Prop.run(p2)
val p2b = Prop.check {
  Par.equal(
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )(ES).get
}

Prop.run(p2b)

val S = Gen.weighted(
  Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
  Gen.unit(Executors.newCachedThreadPool) -> .25)

def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
  Prop.forAll(S ** g) { case s ** a => f(a)(s).get }
def checkPar(p: => Par[Boolean]): ExecutorService => Prop =
  es => Prop { (_, _, _) =>
    if (p(es).get) Proved else Falsified("()", 0)
  }

val p2c = checkPar {
  Par.equal (
    Par.map(Par.unit(1))(_ + 1),
    Par.unit(2)
  )
}

Prop.run(p2c(ES))

val pint = Gen.choose(0, 10).map(Par.unit(_))
val p4 = forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
Prop.run(p4)

// Exercise 16
def generate_par_int(size: Gen[Int]): Gen[Par[Int]] = {
  def gen_of_size(size: Int): Par[Int] =
    if (size <= 1) Par.unit(size)
    else {
      val left = gen_of_size(size / 2)
      val right = gen_of_size(size - size / 2)
      Par.map2(left, right)(_ + _)
    }
  size map gen_of_size
}

val pint2 = generate_par_int(Gen.choose(50, 100))
val p4_2 = forAllPar(pint2)(n => Par.equal(Par.map(n)(y => y), n))
Prop.run(p4_2)

// Exercise 17
val pfork = forAllPar(pint2)(n => Par.equal(Par.fork(n), n))
Prop.run(pfork)

