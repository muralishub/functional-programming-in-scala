package chapterThree.datastructures.Exercise

import chapterThree.datastructures._

/**
  * Created by murali on 17/11/2016.
  */
class Functions {

  def tail[A](list: List[A]): List[A] =

    list match {
      case Cons(head, tail) => tail
      case _ => Nil
    }


  def setHead[A](list: List[A], element: A): List[A] =

    list match {
      case Nil => List(element)
      case Cons(head, tail) => Cons(element, list)
    }


  def drop[A](l: List[A], n: Int): List[A] = {

     if(n == 0) l
     else {
       l match {
         case Cons(head, tail) => drop(tail, n - 1)
         case Nil => l
       }
    }
  }
}



