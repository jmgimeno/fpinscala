import fpinscala.answers.errorhandling.AccumulatingErrors.Person
import fpinscala.exercises.datastructures.List

var l: List[Integer] = List.Nil
var l2 = List.Cons(1, List.Nil)
var l3 = List.Cons(2, l2) // [2, 1]

List.sum(l3)

/*
  prod(Cons(3.0, Cons(0.0, Cons(1.0, Nil))))
=
  3.0 * prod(Cons(0.0, Cons(1.0, Nil)))
=
  3.0 * 0.0
=
  0.0
 */

/*
sum(Cons(2, Cons(1, Nil)))
=
  2 + sum(Cons(1, Nil))
=
  2 + 1 + sum(Nil)
=
  2 + 1 + 0
=
  3
 */

val list = List.apply(1, 2, 3, 4, 5)

val list2 = List(1, 2, 3, 4, 5)

List.drop(list2, 0)

List.reverse(list)

List.foldRight(list, 0, _ + _)

List.foldRightViaFoldLeft(list, 0)(_ + _)

List.foldRightViaFoldLeft(list, 0) { (a, acc) =>
  a + acc
}

val mistery: ((Int, Int) => Int) => Int = List.foldRightViaFoldLeft(list, 0)
mistery(_ + _)

val lis1 = List(1, 2, 3)
val lis2 = List(4, 5)
val lis3 = List(6, 7)
val lislis = List(lis1, lis2, lis3)

List.concat(lislis)

List.zipWithTailRec(lis1, lis2)((_,  _))

