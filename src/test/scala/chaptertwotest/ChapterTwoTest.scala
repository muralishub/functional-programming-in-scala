package chaptertwotest

import chapetertwo.{FibonacciNumber, IsSorted}
import org.scalatest.FunSpec
import org.scalatest.Matchers._
/**
  * Created by mraju on 08/11/16.
  */
class ChapterTwoTest extends FunSpec{

  describe("Fibonacci Number") {
    it("returns correct n") {
      val model = new FibonacciNumber()
      model.fib(2) shouldBe 1
      model.fib(8) shouldBe 21
    }
  }
  describe("isSorted"){
    it("works for Int") {
      val m = new IsSorted
      m.isSorted(Array(1, 2)) shouldBe true
    }


    it("works for anonimus function") {
      val m = new IsSorted
      m.isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => if(x <= y) true else false) shouldBe true
      m.isSorted(Array(1, 2, 2, 4), (x: Int, y: Int) => if(x <= y) true else false) shouldBe true
      m.isSorted(Array(1, 2, 5, 2), (x: Int, y: Int) => if(x <= y) true else false) shouldBe false
      m.isSorted(Array("mike", "abc", "abcd"), (x: String, y: String) => if(x <= y) true else false) shouldBe false
      m.isSorted(Array("abc", "mike"), (x: String, y: String) => if(x <= y) true else false) shouldBe true
    }

  }


}
