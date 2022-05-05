import fpinscala.exercises.monoids.*
import Monoid.{*, given}

val intOptionMonoid = optionMonoid[Int]
intOptionMonoid.combine(None, Some(1))
intOptionMonoid.combine(Some(1), None)
intOptionMonoid.combine(Some(2), Some(1))

val dualIntOptionMonoid = dual(intOptionMonoid)
dualIntOptionMonoid.combine(None, Some(1))
dualIntOptionMonoid.combine(Some(1), None)
dualIntOptionMonoid.combine(Some(2), Some(1))

stringMonoid.combine("hola", "patata")
dual(stringMonoid).combine("hola", "patata")


foldMap(List("hola", "patata", "poma"), stringMonoid)(identity)
