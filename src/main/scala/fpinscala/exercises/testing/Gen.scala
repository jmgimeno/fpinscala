package fpinscala.exercises.testing

import fpinscala.answers.state.*

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop:
  self => // self reference to the receiving object in order to
          // use it inside check and not produce infinite recursion
    def check: Boolean
    def &&(that: Prop): Prop =
      new Prop:
        def check: Boolean = self.check && that.check

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

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

    def sample(rng: RNG): A = self.run(rng)._1

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val probG1 = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < probG1 then g1._1 else g2._1)

