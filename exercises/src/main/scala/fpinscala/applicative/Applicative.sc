import java.util.Date
import java.text.{ParseException, SimpleDateFormat}

import fpinscala.applicative.Applicative._
import fpinscala.applicative._

import fpinscala.applicative.Traverse._

val s1 = Stream(1, 2, 3)
val s2 = Stream(4, 5)
val s3 = Stream(6, 7, 8, 9)

val ls = List(s1, s2, s3)

// Exercise 4: sequence ~~ zip
val sl = streamApplicative.sequence(ls).toList

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

def validName(name: String): Validation[String, String] =
  if (name != "")
    Success(name)
  else
    Failure("Name cannot be empty")

def validBirthDate(birthdate: String): Validation[String, Date] =
  try {
    Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
  } catch {
    case pe: ParseException => Failure("Birthdate must be in the form yyyy-MM-dd")
  }

def validPhone(phoneNumber: String): Validation[String, String] =
  if (phoneNumber.matches("[0-9]{10}"))
    Success(phoneNumber)
  else
    Failure("Phone number must be 10 digits")

def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
  validationApplicative.map3(
    validName(name),
    validBirthDate(birthdate),
    validPhone(phone))(
    WebForm(_, _, _))

validWebForm("JM", "1967-10-04", "0123456789")
validWebForm("", "1967-10-04", "0123456789")
validWebForm("", "4-10-1967", "0123456789")
validWebForm("", "4-10-1967", "123456789")

val optionApplicative: Applicative[Option] = new Applicative[Option] {
  override def unit[A](a: => A): Option[A] = Some(a)
  override def map2[A, B, C](oa: Option[A],
                             ob: Option[B])
                            (f: (A, B) => C): Option[C] =
    (oa, ob) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
}

optionApplicative.sequenceMap(Map(1->Some(1), 2->Some(2)))
optionApplicative.sequenceMap(Map(1->None, 2->Some(2)))

val listApplicative: Applicative[List] = new Applicative[List] {
  override def unit[A](a: => A): List[A] = List(a)
  override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
    for {a <- fa; b <- fb } yield f(a, b)
}

val zipApplicative: Applicative[List] = new Applicative[List] {
  override def unit[A](a: => A): List[A] = List(a)
  override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
    fa zip fb map f.tupled
}

val t = Tree(List(1), List(Tree(List(2, 3)), Tree(List(4))))

treeTraverse.sequence(t)(listApplicative)
treeTraverse.sequence(t)(zipApplicative)

treeTraverse.zipWithIndex(t)

treeTraverse.toList(t)

listTraverse.reverse(List(1, 2, 3))
treeTraverse.reverse(t)

val t2 = Tree(List(5, 6), List(Tree(List(7), List(Tree(List(8))))))

treeTraverse.toList(treeTraverse.reverse(t)) ++ treeTraverse.toList(treeTraverse.reverse(t2))
listTraverse.reverse(treeTraverse.toList(t2) ++ treeTraverse.toList(t))

listTraverse.foldLeft(List(1, 2, 3))(List[Int]())((l, i) => i :: l)
