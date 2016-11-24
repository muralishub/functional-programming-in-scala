package chapterThreeTest

import chapter.three._
import chapter.three.exercises.Functions
import chapter.three.samples._

import org.scalatest.FunSpec
import org.scalatest.Matchers._

/**
  * Created by murali on 17/11/2016.
  */
class ExerciseTest extends FunSpec {


  describe("In Function") {

    it("tail function gets the tail from list") {

      val function = new Functions
      function.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
      function.tail(Nil) shouldBe Nil
      function.tail(List("mike")) shouldBe List()
      function.tail(List("mike", "bramhal")) shouldBe List("bramhal")
    }

    it("set head for a list") {

      val function = new Functions
      function.setHead(List(2, 3), 1) shouldBe List(1, 2, 3)
      function.setHead(Nil, "mike") shouldBe List("mike")

    }

    it("remove first n elements of a list") {

      val function = new Functions
      function.drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
      function.drop(List(), 2) shouldBe List()
    }

    it("drop while List matches a predicate") {
      val function = new Functions

      //function.dropWhile(List(1, 2, 3), (x: Int) => x == 2) shouldBe List(3) //TODO example solved in book doesnt passs this test page 37
      function.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) shouldBe List(4, 5)
    }

    it("Book example add list to end of another") {
      val function = new Functions
      function.append(List(1, 2), List(4, 5, 6)) shouldBe List(1, 2, 4, 5, 6)
    }

    it("init function which returns List except last element, this doesnt work just shouows its difficult in this scenario") {
      val function = new Functions
      function.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    }



  }

}
