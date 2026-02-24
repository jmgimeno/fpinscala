import fpinscala.exercises.gettingstarted.PolymorphicFunctions.partial1

/*
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)
 */

// a = 42 Int
// B Int
// C Boolean
val g: Int => Boolean = partial1(42, (a: Int, b: Int) => a > b)

g(54) // 42 > 54

g(32) // 42 > 32



