import fpinscala.answers.applicative.Applicative
import fpinscala.answers.monoids.*
import Applicative.Validated
import Applicative.Validated.validatedApplicative

given Monoid[String] = Monoid.stringMonoid

val optionApp = Applicative.optionMonad
val validatedApp = Applicative.Validated.validatedApplicative

val pairOptionValidated = optionApp.product(validatedApp)

val r1 = pairOptionValidated.unit[Int](42)
val r2 = pairOptionValidated.unit(50)
val e1: (Option[Int], Validated[String, Int]) = (Option.empty[Int], Validated.Invalid[String, Int]("Error1"))
val e2: (Option[Int], Validated[String, Int]) = (Some(1), Validated.Invalid[String, Int](" Error2"))

pairOptionValidated.map2(r1)(r2)(_ + _)
pairOptionValidated.map2(r1)(e1)(_ + _)
pairOptionValidated.map2(e1)(e2)(_ + _)

