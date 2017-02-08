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
  def toList: List[A] = {
 @annotation.tailrec
    def loop(as: Stream[A], list: List[A]): List[A] = as match {
      case Empty => list.reverse
      case Cons(h, t) => loop(t(), h() :: list)
    }
    loop(this, List())
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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
