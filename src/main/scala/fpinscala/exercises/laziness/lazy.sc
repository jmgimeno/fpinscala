def f(n: Int) : Int = {

  lazy val a: Int = ??? // Costly computation
  lazy val b: Int = ??? // Costly computation
  lazy val c: Int = ??? // Costly computation
  lazy val d: Int = ??? // Costly computation
  lazy val e: Int = ??? // Costly computation
  lazy val f: Int = ??? // Costly computation
  lazy val g: Int = ??? // Costly computation
  lazy val h: Int = ??? // Costly computation
  lazy val i: Int = ??? // Costly computation

  // The val is initialized if and only if its value is needed
  if (a + b < c + d) then ???
  else if (f + g > b) then ???
  else ??? // etc.
}

import fpinscala.exercises.laziness.LazyList
import LazyList.*

val ll =
  Cons(
    () => {println("primer"); 42},
    () => Cons(
            () => { println("segon"); 23},
            () => Empty))

println("lazy list creada")

// Cada vegada s'avalua el "crear" el primer element
ll.headOption
ll.headOption

val ll2 =
  cons(
    { println("primer"); 42 },
    cons({ println("segon"); 23 },
         empty))

ll2.exists(_ <= 0)
// Ara no es torne a avaluar doncs el primer exist ja ho ha
// avaluat (els lazy val de cons ho aconsegueixen)
ll2.exists(_ % 2 == 0)







