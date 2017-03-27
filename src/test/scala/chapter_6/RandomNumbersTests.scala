package chapter_6

import org.scalatest.FunSpec

class RandomNumbersTests extends FunSpec{

  it("random number generator") {
    val rng = SimpleRNG(4).nextInt

    println(rng)
    val rng1 = SimpleRNG(4).nextInt

    println(rng1)
    val rng2 = SimpleRNG(4).nextInt

    println(rng2)
  }









}
