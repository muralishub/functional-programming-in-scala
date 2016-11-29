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

  //Example 3.7 : foldright if used for product will return 0.0 if any item in list is 0.0

  //Example 3.9

  def length[A](as: List[A]): Int = {
    val foo = new RecursionAndGeneralization
    foo.foldRight(as, 0)((a, b) => 1 + b)
  }

  //Exercise 3.10

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    def loop(a: List[A], z: B, f: (B, A) => B): B = {
      a match {
        case Nil => z
        case Cons(x, xs) => {
          loop(xs, f(z, x), f)
        }
      }
    }
    loop(as, z, f)
  }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

  //Exercise 3.11 : write sum, product and get the length using foldLeft

  def sum(list: List[Int]): Int = {
    foldLeft(list, 0)((x , y) => x + y)
  }

  def product(list: List[Int]): Int = {
    foldLeft(list, 1)(_ * _)
  }

  def lengthUsingFoldLeft(list: List[Int]): Int = {
    foldLeft(list, 0)((x, y) => x + 1)
  }

  //Exercise 3.12: write reverse of a list

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((x, y) => Cons(y, x))
  }

  //Exercise 3.13: Impliment foldright via foldleft

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b,a) => f(a,b))
  }

  def foldLeftViafoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((A, B) => f(B, A))
  }




}



