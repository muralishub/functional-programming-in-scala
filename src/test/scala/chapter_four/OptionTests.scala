package chapter_four

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class OptionTests extends FunSpec with Option[Int] {
  it("Lift") {
    val absO: Option[Double] => Option[Double] = Option.lift(math.abs)
    println(absO(Some(math.abs(10.2))))


    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try(age.toInt)
      val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
      Option.map2(optAge, optTickets) {
        _ + _
      }
    }

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch {
        case e: Exception => None
      }

  }
}
class ExerciseTest extends FunSpec {

  //Exercise 4.3
  it("map2") {
    Option.map2[Int, Int, Int](None, Some(1))(_ + _) shouldBe None
    Option.map2[Int, Int, Int](Some(1), None)(_ + _) shouldBe None
  }

  //Exercise 4.4 list of Optional values will return Some and if any value has None will return None
  it("sequence") {
    Option.sequence[Int](List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    Option.sequence[Int](List(Some(1), None)) shouldBe None
  }
}


