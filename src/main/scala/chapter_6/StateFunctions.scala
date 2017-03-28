package chapter_6

class StateFunctions {

  //Exercise 6.1 use nextInt to generate random no between 0 and Int.max
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, r) if x >= 0 => (x, r)
    case _ => nonNegativeInt(rng)
  }


  //Exercise 6.2 generate double between 0 and 1
  def double(rng: RNG): (Double, RNG) = rng.nextInt match {
    case (x, r) if x > 1 => (nonNegativeInt(rng)._1 / Int.MaxValue.toDouble , r)
    case  (x, r) => double(r)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = ???
  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???
  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???




}


trait RNG {
  def nextInt: (Int, RNG)
}

//Book Example
case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt      // >>> RIGHT BINARY SHIFT WITH ZERO FILL
    (n, nextRNG)
  }

  def randomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }
  def randomPairWithDifferentPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }
}