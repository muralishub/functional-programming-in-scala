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
    Stream(1,2, 3, 4).drop(2).toList shouldBe List(3, 4)
  }

  //Exercise 5.3 take while that matches a pridicate
  it("takeWhile") {

    Stream(3, 7, 2).takeWhile(_ % 2 == 1).toList shouldBe List(3, 7)

  }
  //Exercise 5.4
  it("forAll") {
    Stream(2, 4, 7).forAll(_ % 2 == 1) shouldBe false
    Stream(3, 5, 7).forAll(_ % 2 == 1) shouldBe true
  }

  //Exercise 5.5 takewhile
  it("takeWhile using foldRight") {
    Stream(3, 7, 2).takeWhileUsingFoldRight(_ % 2 == 1).toList shouldBe Stream(3, 7).toList
    Stream(2).takeWhileUsingFoldRight(_ % 2 == 1).toList shouldBe Stream().toList
  }

  //Exercise 5.6 headOption using foldRight
  it("headOption") {
    Stream(1, 2).headOption shouldBe Some(1)
  }

  //Exercise 5.7 map, filter
  it("map") {
    Stream(1, 2).map(x => x + 1).toList shouldBe List(2, 3)
  }

  it("filter") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  it("append") {
    Stream(1, 2, 3).append(Stream(4)).toList shouldBe List(1, 2, 3, 4)
  }


}



