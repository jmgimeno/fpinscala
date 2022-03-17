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
