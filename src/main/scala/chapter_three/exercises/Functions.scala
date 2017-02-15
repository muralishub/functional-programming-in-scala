package chapter_three.exercises


import chapter_three.samples._

import scala.annotation.tailrec


/**
  * Created by murali on 17/11/2016.
  */
class Functions {

  //Exercise 3.2 remove first element of list
  def tail[A](list: List[A]): List[A] =

  list match {
    case Cons(head, tail) => tail
    case _ => Nil
  }

  //Exercise 3.3 replace first element for a list
  def setHead[A](list: List[A], element: A): List[A] =

  list match {
    case Nil => List(element)
    case Cons(head, tail) => Cons(element, list)
  }

  //Exercise 3.4 removes first n elements
  def drop[A](l: List[A], n: Int): List[A] = {

    if (n == 0) l
    else {
      l match {
        case Cons(head, tail) => drop(tail, n - 1)
        case Nil => l
      }
    }
  }

  //Exercise 3.5 remove until it matches a predicate
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

  //Exercise 3.6: this doesnt work , use of vectors will be a better solution here
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(h, t) => if (t == Nil) l else init(t)
    }
  }

  //Example 3.7 : foldright if used for product will return 0.0 if any item in list is 0.0

  //Example 3.9 length using foldRight

  def length[A](as: List[A]): Int = {
    val foo = new RecursionAndGeneralization
    foo.foldRight(as, 0)((a, b) => 1 + b)
  }

  //Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def loop(a: List[A], z: B, f: (B, A) => B): B = {
      a match {
        case Nil => z
        case Cons(x, xs) => {
          loop(xs, f(z, x), f)
        }
      }
    }
    loop(as, z, f)
  }

  //Book Example
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //Exercise 3.11 : write sum, product and get the length using foldLeft
  def sum(list: List[Int]): Int = {
    foldLeft(list, 0)((x, y) => x + y)
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
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def foldLeftViafoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((A, B) => f(B, A))
  }


  //Exercise 3.14 Implement append in terms of foldLeft or foldRight
  def appendUsingfoldRight[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)((x, y) => Cons(x, y))
  def appendUsingfoldLeft[A](a: List[A], b: List[A]): List[A] = foldLeft(b, a)((x, y) => Cons(y, x))


  //Exercise 3.15 concact list of lists
  def concat[A](list: List[List[A]]): List[A] = list match {
    case Nil => List()
    case Cons(x, xs) => append(x, concat(xs))
  }

  //Exercise 3.16 List that returns List by adding 1 to each element
  def addOne(list: List[Int]): List[Int] = {
    list match {
      case Nil => List()
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }
  }

  def addOneTailRec(list: List[Int]): List[Int] = {
    @tailrec def loop(l: List[Int], newList: List[Int]): List[Int] = {
      l match {
        case Nil => newList
        case Cons(x, xs) => loop(xs, Cons(x + 1, newList))
      }
    }
    loop(reverse(list), List())
  }

  //Exercise 3.17 convert List[Double] to List[String]
  def doubleToString(list: List[Double]): List[String] = {
    list match {
      case Nil => List[String]()
      case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
    }
  }

  //Exercise 3.18 modify each element in a list while maintaining the structure of the list
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  //Exercise 3.19 filter that removes items in list unless they satisfy a predicate
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)
  }

  //Exercise 3.20 flatMap that works like map but returns a list instead of single result
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    foldRight(list, List[B]())((x, y) => append(f(x), y))
  }

  //Exercise 3.21 implement filter using flatMap
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x) => if (f(x)) List(x) else List())
  }

  //Exercise 3.22 Construct a list by adding corresponding elements from 2 lists
  def addElementsInList[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => bs match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), addElementsInList(xs, ys)(f))
      }
    }
  }

  //Exercise 3.23 make above function generic
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => bs match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), addElementsInList(xs, ys)(f))
      }
    }
  }


  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  //Exercise 3.24 has subSequence
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)

    }
  }


//Exercise 3.25 size of leaf and branches
  def size[T](t: Tree[T]): Int = t match {
    case Leaf(_) => 1
    case Branch(x, y) => 1 + size(x) + size(y)
  }
//Exercise 3.26 max from tree
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  //Exercise 3.27 depth of tree
  def depth(t: Tree[Int]): Int = t match {
    case Leaf(x) => 1
    case Branch(x, y) => 1 + (depth(x) max  depth(y))

  }

  //Exercise 3.28 map just like we have map for list that modifies each element in a list
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))

  }


  //Exercise 3.29 generalize above methods using new fold method fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }



}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]



