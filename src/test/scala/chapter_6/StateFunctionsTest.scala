package chapter_6

import org.scalatest.FunSpec
import org.scalatest.Matchers._


/**
  * Created by murali on 28/03/2017.
  */
class StateFunctionsTest extends FunSpec {

  it("nonNegativeInt") {
    val functions = new StateFunctions
    functions.nonNegativeInt(SimpleRNG(1))._1 should be > 0
    functions.nonNegativeInt(SimpleRNG(Int.MaxValue))._1 should be > 0
    functions.nonNegativeInt(SimpleRNG(0))._1 shouldBe  0
  }

  it("double") {
    val functions = new StateFunctions
    functions.double(SimpleRNG(5000))._1 should be >= 0.0
    functions.double(SimpleRNG(5000))._1 should be <= 1.0
    functions.double(SimpleRNG(99999))._1 should be < 1.0
  }

  it("int and Double") {
    val functions =  new StateFunctions
    functions.intDouble(SimpleRNG(0))._1 should be (0, 0.0)
  }

  it("Double  and Int") {
    val functions =  new StateFunctions
    functions.intDouble(SimpleRNG(0))._1 should be (0.0, 0)
  }

  it("Double Double Double") {
    val functions = new StateFunctions
    functions.double3(SimpleRNG(0))._1 should be (0.0, 0.0, 0.0)
  }

  it("Ints") {
    val functions = new StateFunctions
    functions.ints(0)(SimpleRNG(1))._1 shouldBe List()
    functions.ints(3)(SimpleRNG(1))._1.length shouldBe 3
  }


}
