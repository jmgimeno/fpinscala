import fpinscala.exercises.laziness.LazyList
import LazyList.*

val l_eager: LazyList[Int] = LazyList(1, sys.error("boom"), 2)

val l_lazy: LazyList[Int] = cons(1, cons(sys.error("boom"), cons(2, empty)))

l_lazy.headOption
for {
  rest <- l_lazy.tailOption
  rest2 <- rest.tailOption
  third <- rest2.headOption
} yield third


def f(n: Int): Int =
  println(s"f $n")
  n

val lazyList = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))
val normalList = f(1) :: f(2) :: f(3) :: f(4) :: Nil

lazyList.headOption

lazyList.headOption

val firstThree = lazyList.take(3)

firstThree.tailOption

firstThree.tailOption

firstThree.tailOption.flatMap(_.headOption)

val lazyList2 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

val afterThree = lazyList2.drop(3)

afterThree.headOption

val lazyList3 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

val untilThree = lazyList3.takeWhile(_ < 3)

untilThree.headOption

untilThree.tailOption

def sum(ll: LazyList[Int]): Int =
  println("sum")
  ll.foldRight(0) { (a, b) =>
    println("add")
    a + b
  }

val lazyList4 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))
sum(lazyList4)

val lazyList5 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))
lazyList5.forAll(_ < 3)

val lazyList6 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

// val untilThree2 = lazyList6.takeWhile_viaFoldRight(_ < 3)
// untilThree2.headOption
// untilThree2.tailOption

val anotherLL = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

anotherLL.forAll(_ < 2)
anotherLL.forAll(_ < 2)

val anotherLL2 = cons(f(1), cons(f(2), cons(f(3), cons(f(4), empty))))

val tw = anotherLL2.takeWhile(_ <= 2)
tw.headOption

tw.toList

fibs.take(10).toList

fibsViaUnfold.take(10).toList
