package chapter_four

/**
  * Created by mraju on 25/01/17.
  */

//Exercise 4.1 Impliment functions in trait
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }


  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }


  def getOrElse[B >: A](default: => B): B = default match {
    case None => default
    case Some(x: B) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = ob match {
    case None => ob
    case Some(x) => Some(x)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
//Book Example
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

//Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


}





