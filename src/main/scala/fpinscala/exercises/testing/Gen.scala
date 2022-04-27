package fpinscala.exercises.testing

import fpinscala.answers.state.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.targetName

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

import Prop.*
import Result.*

opaque type Prop = (TestCases, RNG) => Result

object Prop:

  opaque type TestCases = Int
  opaque type SuccessCount = Int
  opaque type FailedCase = String

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true

  end Result

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) =>
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(a, e), i)
    }.find(_.isFalsified).getOrElse(Passed)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  extension (self: Prop)
    def &&(that: Prop): Prop = ???
    def ||(that: Prop): Prop = ???

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = ???

end Prop

opaque type Gen[+A] = State[RNG, A]

object Gen:

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))

                              // vvv State.apply
  def boolean: Gen[Boolean] = State(RNG.boolean)
                                    //^^^^^^^ RNG => (Boolean, RNG)
              // vvvv State[RNG, A]
  extension [A](self: Gen[A])

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

  //def flatMap[B](f: A => State[RNG, B]): State[RNG, B] =
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)      //self.flatMap(f) infinite recursion
      //^^^we can call to the extension method defined in the
      //   companion object as a regular method of the object
      //   passing the "extended object" in a first parameter
      //   list

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def sample(rng: RNG): A = self.run(rng)._1 // Added for tests in the worksheet

    def unsized: SGen[A] = ???

    def list: SGen[List[A]] = ???

  end extension

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val probG1 = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < probG1 then g1._1 else g2._1)

end Gen

opaque type SGen[+A] = Int => Gen[A]

