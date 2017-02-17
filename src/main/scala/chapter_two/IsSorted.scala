package chapter_two

/**
  * Created by mraju on 10/11/16.
  */
class IsSorted {

  //Exercise 2.2 issorted for a given list
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(n: Int): Boolean = {
    if(n >= as.length) true
    else if (ordered(as(n - 1), as(n))) loop(n + 1)

      else
        false
    }
    loop(1)
  }


  def isSorted(as: Array[Int]): Boolean = {

    def loop(n: Int): Boolean = {
      if(n >= as.length) true
      else if (as(n - 1) <= as(n)){
        loop(n + 1)
      }
      else
        false
    }
    loop(1)
  }
}
