package chapter_fiveTests

import chapter_five.{Empty, Exercises,Stream}
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExerciseTests[A] extends FunSpec with Stream[A]{

 it("cons") {
   val s = Stream
   s.cons(1, Stream(1, 2)) shouldBe Stream(1, 1, 2)
 }

it("apply") {
  val s = Stream
  s.apply(1, 2, 3) shouldBe Stream(1,2, 3)
}

//Exercise 5.1 convert stream to list
  it("toList") {
    Stream(1, 2).toList shouldBe List(1, 2)
    }
//Exercise 5.2 take and drop
  it("take") {
    Stream(1,2,3,4).take(2).toList shouldBe List(1, 2)
  }

  it("drop") {
    pending
  }


}



