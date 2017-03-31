package chapter_6

import scala.annotation.tailrec

class StateFunctions {

  type Rand[+A] = RNG => (A, RNG)  // these are called state actions or state transitions


  val int: Rand[Int] = _.nextInt

//BookExample
  def unit[A](a: A): Rand[A] = rnd => (a, rnd)
//BookExample
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
//BookExample
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i % 2)


  //Exercise 6.1 use nextInt to generate random no between 0 and Int.max
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -i else i, r)
  }


  //Exercise 6.2 generate double between 0 and 1
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble + 1, r)
  }


  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  @tailrec
    def loop(count: Int, xl: List[Int], rng: RNG): (List[Int], RNG) = {
     if(count == 0) (xl, rng)
     else {
       val (i, r) = rng.nextInt
       loop(count - 1, i :: xl, rng)
     }
    }
    loop(count, List(), rng)
  }

  //Exercise 6.5 use map to reimplement Double
  def doubleUsingMap: Rand[Double] = map(nonNegativeInt)(i => i / Int.MaxValue.toDouble + 1)

  //Exercise 6.6 implement an other version of map where it also works for binary operations that can handle IntDouble etc
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]  = {
    rng => {
      val (a, rng2) = ra(rng)
      val  (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
//Book Example
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))
//Book Example
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
//Book Example
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


  //Exercise 6.7 ok its working for combinations above what about list
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
  }

  //Book Examples
  def nonNegativeLessThanBook(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThanBook(n)(rng)
  }

  //Exercise 6.8 implement flatMap and use it to implement nonNegativeLessThan
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
     val (i, r) = f(rng)
        g(i)(r)
    }
  }

  //Exercise 6.9 reimplement map and map2 interms of flatMap




  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)

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