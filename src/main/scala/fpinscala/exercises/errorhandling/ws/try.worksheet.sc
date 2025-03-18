
import scala.util.Try

def f(a: Int, b: Int): Int = a / b

Try(f(4, 2))

Try(f(4, 0))

val res = Try(f(4, 0))
  
res.map(_ * 4).flatMap(n => Try(f(100, n))).toEither