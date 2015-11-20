import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._

// Exercise 1
Stream(1, 2, 3).toList

// Exercise 2
Stream(1, 2, 3).take(2)
Stream(1, 2, 3).take(2).toList

Stream(1, 2, 3).drop(2)
Stream(1, 2, 3).drop(2).toList

// Exercise 3
Stream(1, 2, 3, 4).takeWhile(_ < 3).toList

// Exercise 4
Stream(1, 2, 3).forAll(_ < 4)
Stream(1, 2, 3).forAll(_ < 2)

// Exercise 5
Stream(1, 2, 3, 4).takeWhile_fr(_ < 3).toList

// Exercise 6
Stream().headOption
Stream(1).headOption
Stream(1, 2).headOption

// Exercise 7
Stream(1, 2, 3).map(_ * 2).toList
Stream(1, 2, 3).filter(_ > 1).toList
Stream(1, 2).append(Stream(3, 4)).toList

Stream(1,2).flatMap(i => Stream(i, i+1)).toList

// Exercise 8
Stream.constant(2).take(4).toList

// Exercise 9
Stream.from(10).take(4).toList

// Exercise 10
Stream.fibs.take(7).toList
Stream.fibs_zw.take(7).toList

// Exercise 11 & 12
Stream.constant_uf(5).take(5).toList
Stream.from_uf(10).take(5).toList
Stream.fibs_uf.take(7).toList

Stream(1, 2, 3).map_uf(_ * 4).toList
Stream(1, 2, 3).take_uf(2).toList
Stream(1, 2, 3).takeWhile_uf(_ < 3).toList
Stream(1, 2, 3).zipWith_uf(Stream(1, 2, 3))(_ * _).toList
Stream(1, 2, 3).zipAll(Stream(4, 5)).toList

// Lazyness
def pry[A](a: A): A = { print("+ "); a }
def s = cons(pry(1), cons(pry(2), cons(pry(3), empty)))
s.take(0).toList
s.take(1).toList
s.drop(1).toList
s.drop(1).take(1).toList
s.takeWhile(_ < 1).toList
s.takeWhile(_ < 2).toList
s.forAll(_ < 2)
s.headOption
s.map(_ + 1).headOption
s.map(_ + 1).take(0).toList
s.map(_ + 1).take(1).toList
// By-value & by-name & anonymous functions
def g(z: => Int)(f:( => Int) => Int): Int = f(z)
g(pry(1))(i => i + i + i + i)
def h(i: Int) = i + i + i + i
g(pry(1))(h(_))
// g({println("+"); 1})(h)
def k(i: => Int) = i + i + i + i
g(pry(1))(k(_))
g(pry(1))(k)
