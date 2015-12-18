package fpinscala
package applicative

import monads.Functor
import state._
import monoids._
import scala.language.{reflectiveCalls, higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] { self =>

  // Exercise 2
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit[A=>B=>C](f.curried))(fa))(fb)

  def _map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    map(product(fa, fb))(f.tupled)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A=>B](f))(fa)

  def _map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  // Exercise 1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))(map2(_,_)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a),fbs)(_ :: _))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match { case (a, (b, c)) => ((a, b), c) }

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))((a, fas) => map2(f(a), fas)((b, as) => if (b) a :: as else as))

  // Exercise 3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) =
    apply(apply(apply(unit[A=>B=>C=>D](f.curried))(fa))(fb))(fc)

  def _map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) =
    apply[C, D](map2(fa, fb)((a, b) => c => f(a, b, c)))(fc)

  def __map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) =
    apply[A, D](map2(fb, fc)((b, c) => a => f(a, b, c)))(fa)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E) =
    apply(apply(apply(apply(unit[A=>B=>C=>D=>E](f.curried))(fa))(fb))(fc))(fd)

  // Exercise 8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))
      override def map[A, B](faga: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) =
        faga match { case (fa, ga) => (self.map(fa)(f), G.map(ga)(f)) }
      override def map2[A, B, C](faga: (F[A], G[A]), fbgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (faga, fbgb) match {
          case ((fa, ga), (fb, gb)) => (self.map2(fa, fb)(f), G.map2(ga, gb)(f))
        }
    }

  // Exercise 9
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))
      override def map[A, B](fga: F[G[A]])(f: (A) => B): F[G[B]] =
        self.map(fga)(G.map(_)(f))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }

  // Exercise 12
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map[K, V]())){
      case ((k, fv), fm) =>
        map2(fm, fv)((m, v) => m + (k -> v)) }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {

  // Exercise 5
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      ma match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }

  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Exercise 20
  def composeM[F[_],G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]):
    Monad[({type f[x] = F[G[x]]})#f] = new Monad[({type f[x] = F[G[x]]})#f] {
    override def unit[A](a: => A): F[G[A]] =
      F.unit(G.unit(a))
    override def join[A](fgfga: F[G[F[G[A]]]]): F[G[A]] =
      F.map(F.flatMap(fgfga)(gfga => T.sequence(gfga)))(G.join)
  }
}

case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
    OptionT( M.flatMap(value) {
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A)
  extends Validation[Nothing, A]

object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  // Exercise 6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A],
                                 fb: Validation[E, B])
                                (f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
        case (f@Failure(h, t), _) => f
        case (_, f@Failure(h, t)) => f
        case (Success(a), Success(b)) => Success(f(a, b))
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  // Exercise 14
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.getState[S]
      (b, s2) = f(a, s1)
      _ <- State.setState(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // Exercise 16
  def reverse[A](fa: F[A]): F[A] = {
    val ras = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2
    mapAccum(fa, ras){
      case (_, a :: as) => (a, as)
      case _ => sys.error("never")
    }._1
  }

  // Exercise 17
  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  // Exercise 18
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  // Exercise 19
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def map[A, B](fga: F[G[A]])(f: (A) => B): F[G[B]] =
        self.map(fga)(ga => G.map(ga)(f))
      override def traverse[H[_] : Applicative, A, B](fga: F[G[A]])(f: (A) => H[B]): H[F[G[B]]] =
        self.traverse(fga)((ga: G[A]) => G.traverse(ga)(f))

    }
}

case class Tree[+A](head: A, tail: List[Tree[A]] = List())

object Traverse {

  // Exercise 13
  val listTraverse = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](as: List[A])(f: (A) => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
    }
//    override def traverse[G[_], A, B](as: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
//      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      fa match {
        case None    => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
    }
  }
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: (A) => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      G.map2(f(fa.head), G.sequence(fa.tail.map(traverse(_)(f))))(Tree(_, _))
    }
  }
  def mapTraverse[K] = new Traverse[({type l[x] = Map[K, x]})#l] {
    override def traverse[G[_] : Applicative, A, B](fa: Map[K, A])(f: (A) => G[B]): G[Map[K, B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldRight(G.unit(Map[K,B]())){
        case ((k, a), gmkb) =>
          G.map2(f(a), gmkb)((b, mkb) => mkb + (k -> b))}
    }
  }
}

