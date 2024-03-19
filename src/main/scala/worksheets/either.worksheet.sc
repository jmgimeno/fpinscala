import fpinscala.exercises.errorhandling.Either

import Either.*

val ok1 = Right(1)
val ok2 = Right(2)
val err = Left("error")

ok1.orElse(Left(sys.error("boom")))

err.orElse(Left(sys.error("boom")))

ok1.orElse(ok2)
