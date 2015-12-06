package fpinscala.parsing

import language.higherKinds

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice

    def strip[A](p: Parser[A]): Parser[A] = for {
      _ <- spaces
      a <- p
      _ <- spaces
    } yield a

    // Exercise 9
    val pnull: Parser[JSON] =
      strip(string("null")) map (_ => JNull)

    val pnumber: Parser[JSON] =
      strip("\\d+(\\.\\d*)?".r) map (n => JNumber(n.toDouble))

    val pbool: Parser[JSON] =
      strip("true" | "false") map (b => JBool(b.toBoolean))

    val ppair = for {
      k <- strip("[A-Za-z]+".r)
      _ <- char(':')
      v <- pjson
    } yield (k, v)

    val parray = for {
      _ <- strip(char('['))
      l <- (for {
        v <- pjson
        _ <- char(',') | succeed("")
      } yield v).many
      _ <- strip(char(']'))
    } yield JArray(l.toIndexedSeq)

    def pjson: Parser[JSON] = pnull | pnumber | pbool | parray

    pjson
  }

}
