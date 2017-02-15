package chapter_five


class Exercises {

  //Book Example
  def if2[A](cond: Boolean, test : => A, onFalse: => Stream[A]):  A = {
    if(cond)  test
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
    case Cons(h, t) if(n > 1) => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) => Stream.cons(h(), Empty)
  }

  def drop(n: Int): Stream[A] =  {
    def loop(o: Stream[A] ,acc: Int ): Stream[A] = o match {
      case Empty =>  Empty
      case Cons(h, t) if(acc < n) => loop(t(), acc + 1)
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
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match{
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  //BookExample exists using foldRight
  def existsUsingFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)


  //Exercise 5.4 Check elements in a Stream that matches a predicate
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)


   //Exercise 5.5 foldRight to implement takeWhile , take while it mathces a predicate
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((x, y) => if(p(x)) Stream.cons(x,y) else y)



}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:() => A, t:() => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head =  hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def headOption[A](s: Stream[A]): Option[A] = s match {
    case Empty => None
    case Cons(h, t) => Some(h())

  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

}
