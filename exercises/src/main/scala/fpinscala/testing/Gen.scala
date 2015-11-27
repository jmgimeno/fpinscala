package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases, RNG) => Result) {

  // Exercise 9
  def &&(p: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case f => f
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rng) => {
      run(n, rng) match {
        case Passed => Passed
        case _ => p.run(n, rng)
      }
    }
  }
}

object Prop {

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

  //Book's version
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop {
      (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
      }.find(_.isFalsified).getOrElse(Passed)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n\t${e.getStackTrace.mkString("\n\t")}"
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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen {
      State.sequence(List.fill(n)(g.sample))
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
}

case class Gen[A](sample: State[RNG, A]) {

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

  def listOfN_for(size: Gen[Int]): Gen[List[A]] =
    for {
      s <- size
      l <- Gen.listOfN(s, this)
    } yield l

  def listOfN_fm(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
}

trait SGen[+A] {

}

