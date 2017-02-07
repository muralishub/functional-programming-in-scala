package chapter_fiveTests

import chapter_five.Exercises
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class ExerciseTests extends FunSpec{

  it("if2") {
val e = new Exercises
    e.if2(false, sys.error(""), 3) shouldBe 3

    def maybeTwice(b: Boolean, i: => Int) = {if (b) i+i else 0}
    def maybeTwice2(b: Boolean, i: => Int) = {
      lazy val j = i
      if (b) j+j else 0
    }

   // val c = maybeTwice(true, {println("print this"); 1 + 1})
    val c = maybeTwice2(true, {println("print this"); 1 + 1})

println(c)
  }



}



