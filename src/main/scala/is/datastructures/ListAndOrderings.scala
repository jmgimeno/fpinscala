package is.datastructures

import fpinscala.exercises.datastructures.List
import List.*

import scala.annotation.tailrec

object ListAndOrderings {

  // Ordering[A] és un trait que representa el criteri d'ordenació d'elements de tipus A
  // It's an extension of java.util.Comparator[A]
  // We demand an Ordering[B] where B is a supertype of A due to contra-variance (in Java
  // we'd have used Comparator<? super A>)

  def min1[A, B >: A](l: List[A], ord: Ordering[B]): A = {

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if ord.compare(head, min) < 0 => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main1(): Unit = {
    val l = List(3, 4, 1, 2, 5)
    val min = min1(l, Ordering.Int)
    println(s"The min of $l is $min ")
    val max = min1(l, Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }

  // ------------------------------------------------------------------------

  // We can define the parameters in different lists
  // BTW ord.compare(head, min) < 0 can be written as ord.lt(head, min)
  def min2[A, B >: A](l: List[A])(ord: Ordering[B]): A = {

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if ord.lt(head, min) => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main2(): Unit = {
    val l = List(3, 4, 1, 2, 5)
    val min = min2(l)(Ordering.Int)
    println(s"The min of $l is $min ")
    val max = min2(l)(Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }

  // ------------------------------------------------------------------------

  // We can define the parameter as "implicit", that way callers of the function
  // won't need to provide it.
  // But, where this parameter comes from?
  // From the context. If in the context of the call there is an implicit value
  // for the needed type, it is passed as parameter "implicitly".

  def min3[A, B >: A](l: List[A])(using ord: Ordering[B]): A = {

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if ord.lt(head, min) => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main3(): Unit = {
    // There is an implicit value of type Ordering[Int] that is passed to the min3 method
    // This value is: Ordering.Int that cap be passed explicitly (in the example, reversing
    // the criteria)
    val l = List(3, 4, 1, 2, 5)
    val min = min3(l)
    val l2 = List("a", "b")
    println(s"The min of $l is $min ")
    val max = min3(l)(using Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }

  // ------------------------------------------------------------------------

  // Scala allows ,with the magic of extensions (well, really they are implicit conversions
  // because the library works in Scala 2), to use th relational operators <, >, <=, >= between A's
  // when there is an Ordering[A] in scope.
  // To do this we do the import Ordering.Implicits.infixOrderingOps (usually at file level)

  // But in this case, the Ordering we have is for B's and the compiler does not use it to create
  // the extensions (implicit conversions for A). But we can do it manually, that is, create an
  // implicit Ordering for A's that uses the implicit Ordering for N's. Now, we can use the
  // operator.

  def min4[A, B >: A](l: List[A])(using ord: Ordering[B]): A = {

    import Ordering.Implicits.infixOrderingOps

    given Ordering[A] = (a1: A, a2: A) => ord.compare(a1, a2)

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if head < min => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main4(): Unit = {
    // There is an implicit value of type Ordering[Int] that is passed to the min3 method
    // This value is: Ordering.Int that cap be passed explicitly (in the example, reversing
    // the criteria)
    val l = List(3, 4, 1, 2, 5)
    val min = min4(l)
    println(s"The min of $l is $min ")
    val max = min4(l)(using Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }

  // ----------------------------------------------------------------------------

  // We can put the type-class in the parameters list as a context-bound.
  // As we do not have a name for the implicit, if we need it explicitly, we must summon it.

  def min5[A, B >: A : Ordering](l: List[A]): A = {

    import Ordering.Implicits.infixOrderingOps

    given Ordering[A] = (a1: A, a2: A) => summon[Ordering[B]].compare(a1, a2)

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if head < min => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main5(): Unit = {
    // There is an implicit value of type Ordering[Int] that is passed to the min3 method
    // This value is: Ordering.Int that cap be passed explicitly (in the example, reversing
    // the criteria)
    val l = List(3, 4, 1, 2, 5)
    val min = min5(l)
    println(s"The min of $l is $min ")
    val max = min5(l)(using Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }

  // ----------------------------------------------------------------------------

  // Similarly to Java we can use wildcards if we do not need the type name.

  def min6[A](l: List[A])(using ord: Ordering[? >: A]): A = {

    import Ordering.Implicits.infixOrderingOps

    given Ordering[A] = (a1: A, a2: A) => ord.compare(a1, a2)

    @tailrec
    def go(min: A, rest: List[A]): A = {
      rest match
        case Nil => min
        case Cons(head, tail) if head < min => go(head, tail)
        case Cons(_, tail) => go(min, tail)
    }

    l match
      case Nil => sys.error("min of empty list")
      case Cons(head, tail) => go(head, tail)
  }

  @main def main6(): Unit = {
    // There is an implicit value of type Ordering[Int] that is passed to the min3 method
    // This value is: Ordering.Int that cap be passed explicitly (in the example, reversing
    // the criteria)
    val l = List(3, 4, 1, 2, 5)
    val min = min6(l)
    println(s"The min of $l is $min ")
    val max = min6(l)(using Ordering.Int.reverse)
    println(s"The max of $l is $max ")
  }
}

