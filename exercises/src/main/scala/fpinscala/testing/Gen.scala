package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case f => f
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case notF @ (Passed | Proved) => notF
        case _ => p.run(max, n, rng)
      }
    }
  }
}

object Prop {

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n\t${e.getStackTrace.mkString("\n\t")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop {
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
      }.find(_.isFalsified).getOrElse(Passed)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n$msg.");
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop =
    Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
    }
}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample map f)

  // Exercise 6

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen {
      for {
        a <- sample
        b <- f(a).sample
      } yield b
    }

  def flatMap_fm[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] =
    Gen {
      State.sequence(List.fill(n)(sample))
    }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN(_))

  def listOfN_for(size: Gen[Int]): Gen[List[A]] =
    for {
      s <- size
      l <- listOfN(s)
    } yield l

  // Exercise 10
  def unsized: SGen[A] =
    SGen(_ => this)

  def map2[B,C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      a <- this
      b <- gb
    } yield f(a, b)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}



// Exercise 4
object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen {
      for {
        d <- State(RNG.double)
      } yield (d * (stopExclusive - start) + start).toInt
    }

  def choose_basic(start: Int, stopExclusive: Int): Gen[Int] =
    Gen {
      State { rng =>
        val (d, newRng) = RNG.double(rng)
        val i = d * (stopExclusive - start) + start
        (i.toInt, newRng)
      }
    }

  // Exercise 5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen {
      for {
        i <- State(RNG.int)
      } yield i % 2 == 0
    }
  }

  // Exercise 7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    for {
      b <- boolean
      a <- if (b) g1 else g2
    } yield a

  // Exercise 8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    // Assume all weights are positive
    val sumW = g1._2 + g2._2
    val limit = g1._2 / sumW
    for {
      d <- Gen(State(RNG.double))
      a <- if (d <= limit) g1._1 else g2._1
    } yield a
  }

  // Exercise 12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { s => g.listOfN(s) }

  // Exercise 13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen { s => g.listOfN(s max 1) }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen {
      State.sequence(List.fill(n)(g.sample))
    }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  // Exercise 11
  def map[B](f: A => B): SGen[B] =
    SGen { forSize andThen (_ map f) }

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen { forSize andThen (_ flatMap f) }

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))

}




