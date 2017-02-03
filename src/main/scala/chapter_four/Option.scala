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

  //Exercise 4.2 variance function in terms of flatmap
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m , 2))))
  }

  //Note 1: Use map to do something to a list
  //Note 2: use flatMap for multiple stages. if in case of failure computation will abort it wont execute rest of the expression
  //Note 3: getOrElse :  if we have multiple stages we can use it at the end of all states , converts from Option[String] to String
  //Note 4: orElse: execute other condition if first condition is undefined and return Option[String].



  //Book Example : this can operate on option instead of normal values in function
  def Lift[A, B](f: A => B): Option[A] => Option[B] = _ map f


  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(aa => b map(bb => f(aa, bb)))






    a flatMap(aa => b map (bb => f(aa, bb)))

    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

}





