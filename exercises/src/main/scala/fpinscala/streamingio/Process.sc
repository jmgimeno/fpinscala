import fpinscala.streamingio.SimpleStreamTransducers.Process._

import scala.sys.process.Process

val even = filter((x: Int) => x % 2 == 0)

val evens = even(Stream(1, 2, 3, 4)).toList

val take2 = take[Int](2)

take2(Stream()).toList
take2(Stream(1)).toList
take2(Stream(1, 2, 3, 4)).toList

val drop2 = drop[Int](2)

drop2(Stream()).toList
drop2(Stream(1)).toList
drop2(Stream(1, 2, 3, 4)).toList

val takeWhile2 = takeWhile[Int](_ <= 2)

takeWhile2(Stream()).toList
takeWhile2(Stream(1)).toList
takeWhile2(Stream(1, 2, 3, 4)).toList

val dropWhile2 = dropWhile[Int](_ < 2)

dropWhile2(Stream()).toList
dropWhile2(Stream(1)).toList
dropWhile2(Stream(1, 2, 3, 4)).toList

count(Stream("a", "b", "c", "d")).toList

mean(Stream(1.0, 2.0, 3.0, 4.0)).toList

sum2(Stream(1.0, 2.0, 3.0, 4.0)).toList

count3(Stream(1.0, 2.0, 3.0, 4.0)).toList

val add1 = lift[Int,Int](_ + 1)

add1(Stream(1, 2, 3, 4)).toList

val odd = id[Int].filter(_ % 2 == 1)

odd(Stream(1, 2, 3, 4, 5, 6, 7)).toList

val pipeline = filter[Int](_ % 2 == 0) |> lift(_ + 1)

pipeline(Stream(1, 2, 3, 4)).toList

val zip = id[String].zipWithIndex

zip(Stream("a", "b", "c", "d")).toList

val mean_zip = zipWith(sum, count[Double])(_/_)

mean_zip(Stream(1.0, 2.0, 3.0, 4.0)).toList

exists((i: Int) => i % 2 == 0)(Stream(1, 3, 5, 6, 7)).toList

existsResult((i: Int) => i % 2 == 0)(Stream(1, 3, 5, 6, 7)).toList
