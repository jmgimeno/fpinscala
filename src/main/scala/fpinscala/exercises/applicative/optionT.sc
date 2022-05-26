import fpinscala.answers.applicative.OptionT
import fpinscala.answers.applicative.Monad

given Monad[List] with
  def unit[A](a: => A): List[A] = List(a)
  extension [A](as: List[A])
    override def flatMap[B](f: A => List[B]): List[B] =
      as.flatMap(f)

type LO[A] = OptionT[List, A]  // List[Option[A]]

val loMonad = summon[Monad[LO]]

val r1 = OptionT(List(Option.empty[Int], Some(15)))
val r2 = OptionT(List(Some(1), None))

loMonad.map(r1)(_ + 2)

loMonad.map2(r1)(r2)(_ + _)
