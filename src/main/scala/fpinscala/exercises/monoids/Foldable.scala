package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual, listMonoid}

  // We can translate the implementation that we did
  // for the case of List but now they work on any F[_]
  // that is traversable.

  // NOTE: If we define foldMap via foldRight instead of
  // via foldRight (which is usually more common), we have to
  // use the dual monoids in the implementations of foldRight
  // and foldLeft.

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldMap(a => b => f(a, b))(using dual(endoMonoid))(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldMap(b => a => f(a, b))(using endoMonoid)(acc)

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldLeft(mb.empty)((acc, a) => mb.combine(acc, f(a)))

    def combineAll(using ma: Monoid[A]): A =
      as.foldMap(identity)(using ma)

    def toList: List[A] =
      foldMap(a => List(a))(using listMonoid)

object Foldable:

  // For lists we define foldRight/foldLeft as base, so we
  // gain foldMap and combineAll.
  // We also implement toList to get a more efficient implementation
  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] =
        as

  // For IndexedSeq we use the foldLeft/foldRight they have
  // defined in the type and the balances foldMapV that we
  // implemented in Monoid (more efficient than the element
  // by element version)
  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  // For LazyList we use the foldLeft/foldRight they have
  // implemented and get the implementations of the other
  // methods as defined in the trait
  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        ???
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        ???
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        ???
