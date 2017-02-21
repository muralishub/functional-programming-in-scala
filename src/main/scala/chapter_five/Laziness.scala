package chapter_five

import scala.annotation.tailrec


class Laziness {

  //Book Example
  def if2[A](cond: Boolean, test: => A, onFalse: => Stream[A]): A = {
    if (cond) test
    else test
  }
}


//Book Example
trait Stream[+A] {


  //Exercise 5.1 convert Stream to List
  def toList: List[A] = {
    @annotation.tailrec
    def loop(as: Stream[A], list: List[A]): List[A] = as match {
      case Empty => list.reverse
      case Cons(h, t) => loop(t(), h() :: list)
    }
    loop(this, List())
  }

  //Exercise 5.2 take(n) and drop(n)

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) => Stream.cons(h(), Empty)
  }

  def drop(n: Int): Stream[A] = {
    def loop(o: Stream[A], acc: Int): Stream[A] = o match {
      case Empty => Empty
      case Cons(h, t) if acc < n => loop(t(), acc + 1)
      case Cons(h, t) => o
    }
    loop(this, 0)
  }
  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  //BookExample
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //BookExample foldRight for Stream with laziness
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  //BookExample exists using foldRight
  def existsUsingFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)


  //Exercise 5.4 Check elements in a Stream that matches a predicate
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)


  //Exercise 5.5 foldRight to implement takeWhile , take while it mathces a predicate
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((x, y) => if (p(x)) Stream.cons(x, y) else y)


  //Exercise 5.6 HeadOption using foldRight
  def headOption: Option[A] = foldRight(None: Option[A])((x, y) => Some(x))

  //Exercise 5.7 map, filter , append and flatMap using foldRight
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((x, y) => Stream.cons(f(x), y))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((x, y) => if (f(x)) Stream.cons(x, y) else y)

  def append[B >: A](f: => Stream[B]): Stream[B] = foldRight(f)((x, y) => Stream.cons(x, y))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((x, y) => f(x).append(y))

  //Exercise 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //Exercise 5.8 more efficient solution as its one object refering it self
  def constant1[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    t
  }

  //Exercise 5.9 infinite stream of integers starting form n and n +1 n+2 and so on
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def from2(n: Int): Stream[Int] = {
    lazy val t: Stream[Int] = Cons(() => n, () => t.map(x => x + 1))
    t
  }

  //Exercise 5.10 Fibonacci numbers stream 0, 1, 1, 2, 3, 5, 8
  def fibs: Stream[Int] = {
    def loop(i: Int, acc: Int): Stream[Int] = {
      Stream.cons(i, loop(acc, i + acc))
    }
    loop(0, 1)
  }

  //Exercise 5.11 this takes initial value and then a function that produces next state and value in the generated stream
  //this method is corecursive function meaning that it produces data instead of recursive data where it consumes data
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((x, y)) => Stream.cons(x, unfold(y)(f))
      case None => Empty
    }
  }

  //Exercise 5.12 Write fibs, from, constant, and ones in terms of unfold
  def fibsUsingUnfold: Stream[Int] = unfold((0, 1)) { case (x, y) => Some(x, (y, x + y))}

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))


  def constantUsingUnfold[A](n: A): Stream[A] = unfold(n)(x => Some(x, x))

  def onesUsingUnfold: Stream[Int] = unfold(1)(x => Some(1, 1))

  //Exercise 5.13 Write map, take, takeWhile, zipWith and zipAll

  def map[B](f:A => B): Stream[B] = ???

}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def headOption[A](s: Stream[A]): Option[A] = s match {
    case Empty => None
    case Cons(h, t) => Some(h())

  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

}
