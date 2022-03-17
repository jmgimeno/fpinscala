import scala.util.Try

case class Name private (value: String)

object Name:
  def apply(name: String): Either[String, Name] =
    if name == "" || name == null then Left("Name is empty.")
    else Right(new Name(name))

// Sense privat
val name = Name("patata")
val other = new Name("patata")
val empty = Name("")
val nul = Name(null)

// apply

object Doubler:
  def apply(n: Int) = 2 * n

Doubler.apply(25)
Doubler(25)

Try.apply(4 / 0).map(2 * _)



