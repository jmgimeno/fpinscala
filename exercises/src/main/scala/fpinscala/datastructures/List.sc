import fpinscala.datastructures._

val l = List(1, 2, 3, 4)

// Exercise 1
List.x

// Exercise 2
List.tail(l)

// Exercise 3
List.setHead(l, 5)

// Exercise 4
List.drop(l, 2)

// Exercise 5
List.dropWhile(l, (x:Int) => x <= 2)

// Exercise 6
List.init(l)

// Exercise 8
List.foldRight(l, Nil:List[Int])(Cons(_, _))

// Exercise 9
List.length(l)

// Exercise 10, 11
List.sum_fl(l)
List.product_fl(l)
List.length_fl(l)

// Exercise 12
List.reverse_fl(l)

// Exercise 13
List.foldRight_fl(l, Nil:List[Int])(Cons(_, _))
List.foldRight_fl_2(l, Nil:List[Int])(Cons(_, _))
List.foldLeft_fr(l, Nil:List[Int])((t, h) => Cons(h, t))

// Exercise 14
List.append_fr(l, List(5, 6, 7))

// Exercise 15
List.flatten_fr(List(List(1, 2, 3), Nil, List(4, 5, 6)))

// Exercise 16
List.inc_each(l)

// Exercise 17
List.toString_each(l)

// Exercise 18
List.map(l)(_ * 2)

// Exercise 19
List.filter(l)(_ % 2 == 0)

// Exercise 20
List.flatMap(l)(i => List(i, i))
List.flatMap_2(l)(i => List(i, i))

// Exercise 21
List.filter_fm(l)(_ % 2 == 0)

// Exercise 22
List.add_lists(List(1,2,3),
               List(4,5,6))

// Exercise 22
List.zipWith(List(1,2,3), List(4,5,6))(_ * _)
