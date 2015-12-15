import java.util.Date
import java.text.{ParseException, SimpleDateFormat}

import fpinscala.applicative.Applicative._
import fpinscala.applicative.{Validation, Success, Failure}

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
