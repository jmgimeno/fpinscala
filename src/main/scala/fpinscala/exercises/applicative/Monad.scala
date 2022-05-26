package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    override def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  override def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ff.flatMap(f => fa.map(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

object Monad:

  def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[x] =>> G[H[x]]] = new:
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    extension [A](gha: G[H[A]])
      override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
        //val ghghb: G[H[G[H[B]]]] = G.map(gha)(ha => H.map(ha)(f))
        //val gghhb: G[G[H[H[B]]]] = G.map(ghghb)(hghb => T.traverse(hghb)(ghb => ghb))
        //val ghhb: G[H[H[B]]] = G.join(gghhb)
        //val ghb: G[H[B]] = G.map(ghhb)(hhb => H.join(hhb))
        //ghb
        G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))

  /*
  The official solution is much simpler and elegant but can be found from our solution:

    G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))

  * map the result of a map is simply the map of the composite funcion
      x.map(f).map(g) = x.map(f andThen g)

  * traverse the result of a map is a single traverse over the composition of the map
      x.map(f).traverse(g) = x.traverse(f andThen g)

  val gghhb = G.map(G.map(gha)(ha => H.map(ha)(f)))(hghb => T.traverse(hghb)(ghb => ghb))
            = G.map(gha)(ha => T.traverse(ha)(f))

  * a map followed by a join is a single flatMap
      x.map(f).join = x.flatMap(f)

  val ghhb = G.flatMap(gha)(ha => T.traverse(ha)(f))

  * If I map over the result of a flatMap I can map inside
      x.flatMap(f).map(g) = x.flatMap(a => f(a).map(g))

  val ghb = G.map(G.flatMap(gha)(ha => T.traverse(ha)(f)))(hhb => H.join(hhb))
          = G.map(G.flatMap(gha)(ha => T.traverse(ha)(f)))(H.join)
          = G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))
  */

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [A](fa: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]): Either[E, B] =
        fa match
          case Left(e) => Left(e)
          case Right(a) => f(a)
