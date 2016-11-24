package chapter.three.exercises

import chapter.three.samples._


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

    if (n == 0) l
    else {
      l match {
        case Cons(head, tail) => drop(tail, n - 1)
        case Nil => l
      }
    }
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {

    list match {
      case Cons(h, t) if (f(h)) => dropWhile(t, (f))
      case _ => list
    }
  }


  //book exmaple: add list of all elements to end of another
  def append[A](al: List[A], a2: List[A]): List[A] = {
    al match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }
  // this doesnt work , use of vectors will be a better solution here
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(h, t) => if (t == Nil) l else init(t)
    }
  }


}



