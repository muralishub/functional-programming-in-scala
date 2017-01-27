package chapter_four.exercises

/**
  * Created by mraju on 25/01/17.
  */
//Exercise 4.1
trait Option[+A] {
  def map[B](f: A => B): Option[B]


    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): Option[B]
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]


}