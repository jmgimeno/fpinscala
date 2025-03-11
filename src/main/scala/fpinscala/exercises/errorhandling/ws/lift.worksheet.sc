import fpinscala.exercises.errorhandling.Option
import Option.*

val l1: List[Int] = List(1, -2, 3, -5)

// math.abs: Int => Int
l1.map(math.abs)

val l2: List[Option[Int]] = List(Some(1), None, Some(3), Some(-5))

l2.map(lift(math.abs))

"patata".toIntOption
