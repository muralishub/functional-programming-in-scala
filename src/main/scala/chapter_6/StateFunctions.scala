package chapter_6

class StateFunctions {

  //Exercise 6.1 use nextInt to generate random no between 0 and Int.max
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if(i < 0) -i else i, r)
  }


  //Exercise 6.2 generate double between 0 and 1
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue, r)
  }


  //Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
     ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

//  {
//
//    def list: (List[Int], RNG) = {
//     val r = (rng.nextInt._1 :: Nil, rng.nextInt._2)
//      r
//    }
//
//
//
//
//    def loop(count: Int, result: (List[Int], RNG), inc: Int) = {
//      if(count > inc) {
//       loop(count, result :: result._2.nextInt, inc + 1)
//
//      }
//    }
//
//    loop(count, list, 0)
  }




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