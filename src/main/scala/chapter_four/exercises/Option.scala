package chapter_four.exercises

/**
  * Created by mraju on 25/01/17.
  */

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case a: A => Some(f(a))
    case None => None
  }


//  def flatMap[B](f: A => Option[B]): Option[B]
//
//  def getOrElse[B >: A](default: => B): Option[B]
//
//  def orElse[B >: A](ob: => Option[B]): Option[B]
//
//  def filter(f: A => Boolean): Option[A]


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



