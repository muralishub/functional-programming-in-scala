package chapter_7

import scala.annotation.tailrec

/**
  * Created by mraju on 05/04/17.
  */
class Parallelizam {


  //Book Example

  def sum(ints: Seq[Int]): Int = ints.foldLeft(0)((a, b) => (a + b))


def sum(ints: IndexedSeq[Int]): Int = {
  if(ints.length <= 1)
    ints.headOption getOrElse 0
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    sum(l) + sum(r)
  }

}

}
