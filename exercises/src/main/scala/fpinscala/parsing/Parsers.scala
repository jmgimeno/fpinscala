package fpinscala.parsing

import fpinscala.testing.SGen

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  val numA: Parser[Int] = char('a').many.map(_.size)

  // Exercise 8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))

  // Exercise 7
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p flatMap (a => p2 map (b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p flatMap (a => p2 map (b => f(a, b)))

  // Exercise 4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 1) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  // Exercise 3
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def countZeroOrMore_orig(a: Char): Parser[Int] = map(many(char(a)))(_.size)

  def countZeroOrMore(a: Char): Parser[Int] = char(a).many.slice.map(_.length)

  def countOneOrMore(a: Char): Parser[Int] = char(a).many1.slice.map(_.length)

  // Exercise 1
  def map2_ex1[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    (p ** p2) map f.tupled

  // Exercise 6
  def num_of_as: Parser[String] =
    regex("\\d+".r).flatMap(d => listOfN(d.toInt, string("a")).map(l => l.reduce(_ ++ _)))

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def attempt: Parser[A] = self.attempt(p)
  }

  object Laws {

    import fpinscala.testing.{Prop, Gen}
    import fpinscala.testing.Prop.forAll

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def charLaw(in: Gen[Char]): Prop =
      forAll(in)(c => run(char(c))(c.toString) == Right(c))

    def stringLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s) == Right(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](ga: Gen[A], in: Gen[String]): Prop =
      forAll(ga.map2(in)((_, _))) { case (a, s) => run(succeed(a))(s) == Right(a) }

    // Exercise 2
    def productLaw[A, B](p: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      equal(slice(p ** p2), slice(p).map2(slice(p2))(_ ++ _))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }}
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
