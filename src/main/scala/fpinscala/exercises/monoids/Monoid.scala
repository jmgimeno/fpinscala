package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String): String = a1 + a2
    val empty: String = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val empty: List[A] = List.empty[A]

//  val intListMonoid = listMonoid[Int]

//  val intListMonoid: Monoid[List[Int]] = new:
//    def combine(li1: List[Int], li2: List[Int]) = li1 ++ li2
//    val empty = List.empty[Int]
//
//  val stringListMonoid: Monoid[List[String]] = new:
//    def combine(li1: List[String], li2: List[String]) = li1 ++ li2
//    val empty = List.empty[String]

  val intAddition: Monoid[Int] = new:
    def combine(i1: Int, i2: Int): Int = i1 + i2
    val empty: Int = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(i1: Int, i2: Int): Int = i1 * i2
    val empty: Int = 1

  val booleanOr: Monoid[Boolean] = new :
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val empty: Boolean = false

  val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val empty: Boolean = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    val empty: Option[A] = None

  def optionMonoid_combining[A](m: Monoid[A]) : Monoid[Option[A]] = new:
    def combine(o1: Option[A], o2: Option[A]): Option[A] = (o1, o2) match
      case (Some(a1), Some(a2)) => Some(m.combine(a1, a2))
      case (None, o) => o
      case (o, None) => o

    val empty: Option[A] = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty: A = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f1: A => A, f2: A => A): A => A = f1.andThen(f2)
    val empty: A => A = identity

  import fpinscala.answers.testing.{Prop, Gen}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val neutral: Prop = Prop.forAll(gen) { a =>
      m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
    }
    val associativity: Prop = Prop.forAll(gen ** gen ** gen) { case a1 ** a2 ** a3 =>
      m.combine(a1, m.combine(a2, a3)) == m.combine(m.combine(a1, a2), a3)
    }
    neutral && associativity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    // combineAll(as.map(f), m) but too many list traversals !!!
    as.foldLeft(m.empty)((acc, a) => m.combine(acc, f(a)))

  /*
     val l = List(a1, a2)
     via foldRight(acc)(f):
                   a1 f (a2 f acc)
     via foldMap:
                   ((identity andThen f(a1)) andThen f(a2))(acc)

     so I need the first function to be applied be f(a2)
       => I need the dual of the endoMonoid !!!
  */
  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(a => b => f(a, b))(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match
      case 0 => m.empty
      case 1 => f(as(0))
      case n => val (left, right) = as.splitAt(n / 2)
                m.combine(foldMapV(left, m)(f), foldMapV(right, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  case class SortedInfo(sorted: Boolean, min: Int, max: Int)
  val sortedInfoMonoid: Monoid[SortedInfo] = new:
    def combine(left: SortedInfo, right: SortedInfo): SortedInfo =
      val SortedInfo(ls, lmin, lmax) = left
      val SortedInfo(rs, rmin, rmax) = right
      SortedInfo(ls && rs && lmax <= rmin, lmin min rmin, lmax max rmax)
    val empty: SortedInfo = SortedInfo(true, Int.MaxValue, Int.MinValue)

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, sortedInfoMonoid)(i => SortedInfo(true, i, i)).sorted

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new Monoid[WC]:
    import WC.*
    def combine(wc1: WC, wc2: WC): WC = (wc1, wc2) match
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        val wordsInTheMiddle = if (r1 + l2).isEmpty then 0 else 1
        Part(l1, w1 + wordsInTheMiddle + w2, r2)
      case (Part(l1, w1, r1), Stub(s2)) => Part(l1, w1, r1 + s2)
      case (Stub(s1), Part(l2, w2, r2)) => Part(s1 + l2, w2, r2)
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)

    val empty: WC = Stub("")

  def count(s: String): Int =
    import WC.*
    def toWC(c: Char): WC =
      if c.isLetter then Stub(c.toString) else Part("", 0, "")
    def countIfNonEmpty(s: String) = if s.isEmpty then 0 else 1
    foldMapV(s, wcMonoid)(toWC) match
      case Part(l, w, r) => countIfNonEmpty(l) + w + countIfNonEmpty(r)
      case Stub(s) => countIfNonEmpty(s)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) : (A, B) =
      val (a1, b1) = x
      val (a2, b2) = y
      (ma.combine(a1, a2), mb.combine(b1, b2))

    val empty: (A, B) = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B): A => B =
      a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] =
      (a.keySet ++ b.keySet).foldLeft(empty) { (acc,k) =>
        acc.updated(k, mv.combine(a.getOrElse(k, mv.empty),
          b.getOrElse(k, mv.empty)))
      }
    val empty: Map[K, V] = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    val m = mapMergeMonoid[A, Int]
    foldMapV(as, m)(a => Map(a -> 1))

  def bag_2[A](as: IndexedSeq[A]): Map[A, Int] =
    import Foldable.given
    as.foldMap(a => Map(a -> 1))

  // We'll need these given instances
  given _intMonoid: Monoid[Int] = intAddition
  given _listMonoid[A]: Monoid[List[A]] = listMonoid

end Monoid

