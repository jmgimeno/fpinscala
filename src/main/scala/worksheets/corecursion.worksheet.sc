import fpinscala.exercises.laziness.LazyList
import LazyList.*

val nats = from(0)

nats.take(10).toList

val nats2 = fromViaUnfold(0)

nats2.take(10).toList

firstFiveEvenNumbers.toList

fromViaUnfold(1).take(10).foldRight(0)(_ + _)

