package fpinscala.parsing

import MyParserTypes._

import scala.util.matching.Regex

object MyParserTypes {

  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object MyParser extends Parsers[Parser] {

  // Exercise 13
  def string(s: String): Parser[String] = {
    case loc @ Location(input, offset) => input.indexOf(s, offset) match {
      case -1  => Failure(ParseError(List(loc -> "no string")))
      case pos => Success(s, s.length)
    }
  }

  def regex(r: Regex): Parser[String] = ???
  def slice[A](p: Parser[A]): Parser[String] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  def errorMessage(e: ParseError): String = ???



  def errorLocation(e: ParseError): Location = ???

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???
}
