import fpinscala.monoids.Monoid._
import fpinscala.testing.{Prop, Gen}

Prop.run(monoidLaws(stringMonoid, Gen.stringN(5)))
Prop.run(monoidLaws(listMonoid[Int], Gen.listOfN(5, Gen.choose(1, 20))))
Prop.run(monoidLaws(intAddition, Gen.choose(1, 20)))
Prop.run(monoidLaws(intMultiplication, Gen.choose(1, 20)))
Prop.run(monoidLaws(booleanOr, Gen.boolean))
Prop.run(monoidLaws(booleanAnd, Gen.boolean))
val l = List(1, 2, 3, 4)
foldLeft(l)(List[Int]())((l, a) => a :: l)
foldRight(l)(List[Int]())(_ :: _)
foldMapV(l.toIndexedSeq, intAddition)(identity)
foldMapV(l.toIndexedSeq, intMultiplication)(identity)
ordered(l.toIndexedSeq)
val l2 = List(1, 2, 4, 3)
ordered(l2.toIndexedSeq)

count("")
count("loren")
count(" loren")
count("loren ")
count(" loren ")
count("loren ipsum dolor sit amet")
count(" loren    ipsum     dolor     sit     amet    ")


bag(Vector(1, 2, 1, 3, 4, 1, 2))
