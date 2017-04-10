package chapter_7

import scala.annotation.tailrec

/**
  * Created by mraju on 05/04/17.
  */
class Parallelism {


  //Book Example
  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)((a, b) => (a + b))

//Book Example
  def sum(ints: IndexedSeq[Int]): Int = {
    if (ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  }

  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???



//Book Example
//undating sum with our custom data type
    def sumPar(ints: IndexedSeq[Int]): Int = {
      if (ints.length <= 1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        val sumL: Par[Int] = unit(sumPar(l))
        val sumR: Par[Int] = unit(sumPar(r))

        get(sumL) + get(sumR)
      }
    }
//if we inline above sumL and sumR we get same result but its no longer parallel because only after sum is executed get is executed
//this can be  solved by avoiding get and instead return Par[Int] instead of Int
//Book Example
      def sumPar1(ints: IndexedSeq[Int]): Par[Int] = {
        if(ints.length <= 1)
         unit(ints.headOption getOrElse 0)
        else{
          val (l, r) = ints.splitAt(ints.length / 2)
          Par.map2(sumPar1(l), sumPar1(r))(_ + _)
        }
      }
}




  class Par[T] {
  }


  object Par{
    // Exercise 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
  }


