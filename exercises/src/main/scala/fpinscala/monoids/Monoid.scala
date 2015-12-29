package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2
    override val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]) = a1 ++ a2
    override val zero = Nil
  }

  // Exercise 1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2
    override def zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }

  // Exercise 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  // Exercise 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: (A) => A): A => A = a1 compose a2
    override def zero: A => A = identity
  }

  import fpinscala.testing._
  import Prop._

  // Exercise 4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => m.op(a, m.zero) == a) &&
    forAll(gen)(a => m.op(m.zero, a) == a) &&
    forAll(gen ** gen ** gen){ case ((a, b), c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // Exercise 7
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val mid = as.length/2
      val (left, right) = as.splitAt(mid)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  // Exercise 9
  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, new Monoid[(Int, Int, Boolean)] {
      override def op(a1: (Int, Int, Boolean),
                      a2: (Int, Int, Boolean)): (Int, Int, Boolean) = {
        val (min1, max1, ord1) = a1
        val (min2, max2, ord2) = a2
        (Integer.min(min1, min2), Integer.max(max1, max2), ord1 &&  ord2 && max1 <= min2 )
      }
      override def zero: (Int, Int, Boolean) = (Integer.MAX_VALUE, Integer.MIN_VALUE, true)
    })(i => (i, i, true))._3

  // Exercise 8
  import fpinscala.parallelism.Nonblocking._
  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    sys.error("todo")
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    sys.error("todo")


  // Exercise 10 (with some help)
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(left), Stub(right)) =>
        Stub(left + right)
      case (Stub(prefix), Part(left, words, right)) =>
        Part(prefix+left, words, right)
      case (Part(left, words, right), Stub(suffix)) =>
        Part(left, words, right+suffix)
      case (Part(left1, words1, right1), Part(left2, words2, right2)) =>
        Part(left1, words1 + words2 + (if ((right1+left1).isEmpty) 0 else 1), right2)
    }
    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {

    def toWC(c: Char) =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toCharArray.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(chars) => unstub(chars)
      case Part(left, words, right) => unstub(left) + words + unstub(right)
    }
  }

  // Exercise 16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) =
        (A.zero, B.zero)
    }

  // Exercise 17
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B =
        a => B.op(a1(a), a2(a))
      override def zero: A => B =
        _ => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a1.getOrElse(k, V.zero),
                              a2.getOrElse(k, V.zero)))}
      override def zero: Map[K, V] = Map[K, V]()
    }

  // Exercise 18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

// Exercise 12
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 15
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

// Exercise 13
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

// Exercise 14
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.fold(mb.zero)(f)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.fold(z)(f(z, _))
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.fold(z)(f(_, z))
}

