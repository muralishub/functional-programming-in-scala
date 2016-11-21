package chapterThreeTest

import chapterThree.datastructures.Exercise.Functions
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import chapterThree.datastructures._

/**
  * Created by murali on 17/11/2016.
  */
class FunctionsTest extends FunSpec{


  describe("In Function") {

    it("tail function gets the tail from list") {

      val function  = new Functions
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

      function.dropWhile(List(1, 2, 3), (x: Int) => if(x == 2) true else false) shouldBe List(3)
    }



    }

}
