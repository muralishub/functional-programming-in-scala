package chapter_fourTests

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import chapter_four._

class Samples extends FunSpec {
  it("Lift") {

    val s = "dd"

    val r = Try(s.toInt)


    println(r)
    val absO: Option[Double] => Option[Double] = Option.Lift(math.abs)
    println(absO(Some(math.abs(10.2))))
  }


  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    Option.map2(optAge, optTickets){insuranceRateQuote}
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch{case e: Exception => None}

}