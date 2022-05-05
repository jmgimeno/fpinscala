import fpinscala.answers.monoids.Monoid.{*, given}

foldMapG(List(1,2,3,4))(_ * 2)(using intMultiplication)
