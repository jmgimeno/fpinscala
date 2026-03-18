import scala.util.Try

val msg1 = "24"
val msg2 = "42"

for {
  a <- Try(msg1.toInt)
  b <- Try(msg2.toInt)
} yield a + b


