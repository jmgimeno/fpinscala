import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Left
import fpinscala.errorhandling.Right
import fpinscala.errorhandling.Either._

val left1: Either[String, Int] = Left("error1")
val left2: Either[String, Int] = Left("error2")
val right1: Either[String, Int] = Right(1)
val right2: Either[String, Int] = Right(2)

// Exercise 6
left1.map(_ + 1)
right1.map(_ + 1)

left1.flatMap(_ => left2)
left1.flatMap(_ => right1)
right1.flatMap(_ => left1)
right1.flatMap(i => Right(i+1))

left1.orElse(left2)
left1.orElse(right1)
right1.orElse(left1)
right1.orElse(right2)

left1.map2(left2)(_ + _)
left1.map2(right1)(_ + _)
right1.map2(left1)(_ + _)
right1.map2(right2)(_ + _)

// Exercise 5
traverse(List(1, 2, 3))(i => if (i > 2) Left(i) else Right(i + 1))
traverse(List(1, 2, 3))(i => if (i > 3) Left(i) else Right(i + 1))

sequence(List(left1, left2))
sequence(List(left1, right1))
sequence(List(right1, left1))
sequence(List(right1, right2))
