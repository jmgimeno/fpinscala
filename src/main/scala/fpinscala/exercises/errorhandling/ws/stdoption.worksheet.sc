def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

def parseInsuranceRateQuote(age: String,
                            numberOfSpeedingTickets: String): Option[Double] =
  val optAge: Option[Int] = age.toIntOption
  val optTickets: Option[Int] = numberOfSpeedingTickets.toIntOption
  (optAge, optTickets) match
    case (Some(age), Some(tickets)) => Some(insuranceRateQuote(age, tickets))
    case _ => None