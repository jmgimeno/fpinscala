val l = List()
val l2 = 1 :: List()
val l3 = 2 :: l2

val list = List(1, 2, 3, 4, 5)

class Person
class Engineer extends Person

// => List[Engineer] extends List[Person]

val p = List(new Person)
val e = List(new Engineer, new Engineer)

def f(people: List[Person]) =
  people.size

f(p)
f(e)

p

val res = new Person :: e
//        Person     List[Engineer]
//                   List[Person]
// List[Person]

val list2 = new Engineer :: Nil
val list3 = new Person :: list2

var list4: List[Any] = 42 :: list3

def sum(l: List[Int]): Int = l match
  case Nil     => 0
  case x :: xs => x + sum(xs)

sum(list)

val pairs =
  for
    a <- List(1, 2, 3)
    b <- List("a", "b")
  yield (a, b)

val pairs2 =
  List(1, 2, 3).flatMap { a =>
    List("a", "b").map { b =>
      (a, b)
    }
  }
