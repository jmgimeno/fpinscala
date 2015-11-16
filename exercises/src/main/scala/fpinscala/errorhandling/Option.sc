import fpinscala.errorhandling.Option
import fpinscala.errorhandling.None
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.Option._

val none:Option[Int] = None
val some1 = Some(1)
val some2 = Some(2)

// Exercise 1
none.map(_ + 1)
some1.map(_ + 1)

none.getOrElse(0)
some1.getOrElse(2)

none.flatMap(i => Some(i + 1))
some1.flatMap(i => Some(i + 1))

none.orElse(none)
none.orElse(some1)
some1.orElse(none)
some1.orElse(some2)

none.filter(_ > 0)
some1.filter(_ > 0)
some1.filter(_ > 1)

// Exercise 2
variance(Seq())
variance(Seq(1.0, 1.0))
variance(Seq(1.0, 2.0))

// Exercise 3
map2(none, none)(_ + _)
map2(none, some1)(_ + _)
map2(some1, none)(_ + _)
map2(some1, some2)(_ + _)

// Exercise 4
sequence(List(none, some1))
sequence(List(some1, none))
sequence(List(some1, some2))

// Exercise 5
traverse(List(1, 2, 3))(i => if (i > 2) none else Some(i + 1))
traverse(List(1, 2, 3))(i => if (i > 3) none else Some(i + 1))
