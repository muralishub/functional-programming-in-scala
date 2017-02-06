package chapter_five

class Exercises {

  //Book Example
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A) = {
    if(cond) onTrue
    else onFalse
  }


}



//Book Example
sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

}
