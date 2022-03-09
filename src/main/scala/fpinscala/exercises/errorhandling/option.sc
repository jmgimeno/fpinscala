import fpinscala.exercises.errorhandling.Option
import Option.*

val o1: Option[Int] = Some(1)
val o2: Option[String] = Some("1")
val o3: Option[Int] = None

o1.orElse(o2)
o3.orElse(o2)

