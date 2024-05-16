import fpinscala.exercises.state.*

import scala.collection.mutable
import State.*

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Patata:
  val elems = new ListBuffer[String]

  def foo(s: String): Int =
    elems += s
    elems.length + s.length

val p = new Patata
val r1 = p.foo("hola")
val r2 = p.foo("la")
val r3 = p.foo("hola")

def fooAction(s: String): State[List[String], Int] =
  State { (l: List[String]) =>
    (l.length + 1 + s.length, l ++ List(s))
  }

val actions = List(
  fooAction("hola"),
  fooAction("la"),
  fooAction("hola"))

val combinedAction = State.sequence(actions)

combinedAction.run(List())

