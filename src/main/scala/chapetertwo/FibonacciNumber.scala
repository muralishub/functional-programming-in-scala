package chapetertwo


/**
  * Created by mraju on 08/11/16.
  */
class FibonacciNumber {

  def fib(n: Int): Int = {
    def go(first: Int, second: Int, list: List[Int], index: Int): Int = {
     if(list.length == index) first + second
      else
       go(second, first + second, list :+ first + second, index )
    }
    go(0, 1, List(0, 1), n)
  }



}
