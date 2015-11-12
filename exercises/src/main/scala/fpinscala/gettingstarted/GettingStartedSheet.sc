import fpinscala.gettingstarted._

// Exercise 1
TestFib.main(null)


val a = Array(1.0, 2.0, 3.0)

MonomorphicBinarySearch.binarySearch(a, 2.5)

PolymorphicFunctions.binarySearch(a, 2.5, (x: Double, y: Double) => x >= y)

// Exercise 2
PolymorphicFunctions.isSorted(a, (x: Double, y: Double) => x <= y)

val a2 = Array(1.0, 3.0, 2.0)

PolymorphicFunctions.isSorted(a2, (x: Double, y: Double) => x <= y)

PolymorphicFunctions.isSorted[Double](a2, _ <= _)

// Exercise 3
val cadd = PolymorphicFunctions.curry((x:Int, y:Int) => x + y)

val inc = cadd(1)

inc(2)

// Exercise 4
val uadd = PolymorphicFunctions.uncurry(cadd)

uadd(2, 3)

val double = (x: Int) => 2 * x

// Exercise 5
val nextDouble = PolymorphicFunctions.compose(double, inc)

nextDouble(2)
