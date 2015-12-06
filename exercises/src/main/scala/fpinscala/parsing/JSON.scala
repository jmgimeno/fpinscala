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

    // Exercise 9
    val pnull: Parser[JSON] = for {
      _ <- spaces
      _ <- string("null")
      _ <- spaces
    } yield JNull

    val pnumber: Parser[JSON] = for {
      _ <- spaces
      n <- "\\d+(\\.\\d*)?".r
      _ <- spaces
    } yield JNumber(n.toDouble)

    val pbool: Parser[JSON] = for {
      _ <- spaces
      b <- "true" | "false"
      _ <- spaces
    } yield JBool(b.toBoolean)

    val ppair = for {
      _ <- spaces
      k <- "[A-Za-z]+".r
      _ <- spaces
      _ <- char(':')
      _ <- spaces
      v <- pjson
    } yield (k, v)

    val parray = for {
      _ <- spaces
      _ <- char('[')
      _ <- spaces
      l <- (for {
        _ <- spaces
        v <- pjson
        _ <- spaces | char(',')
      } yield v).many
      _ <- spaces
      _ <- char(']')
      _ <- spaces
    } yield JArray(l.toIndexedSeq)

    def pjson: Parser[JSON] = pnull | pnumber | pbool | parray

    pjson
  }

}
