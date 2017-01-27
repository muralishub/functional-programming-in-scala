package chapter_two.exercises

/**
  * Created by mraju on 15/11/16.
  */
class Currying {

  //Exercise: 2.3 converts a function f of two arguments into a function of one argument that partially applies f
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  //Exercise: 2.4 reverses the transformation of a curry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  //Exercise: 2.5 impliment higher order function that composes two functions
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => (f(g(a)))

}
