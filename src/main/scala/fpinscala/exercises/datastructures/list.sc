import fpinscala.exercises.datastructures.List

val l = List(1, 2, 3)

val empty = List.Nil

val one = List.Cons("one", List.Nil)

import List.*

val empty_v2 = Nil

val one_v2 = Cons("one", Nil)

empty == empty_v2

one == one_v2

