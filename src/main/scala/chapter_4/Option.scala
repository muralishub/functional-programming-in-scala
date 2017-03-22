package chapter_4

import chapter_3.book_samples.Cons

/**
  * Created by mraju on 25/01/17.
  */

//Exercise 4.1 Impliment functions in trait
 trait Option[+A] {

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

//BookExample
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f



  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(x => b map(y => f(x, y)))
  }

 // Exercise 4.4 list of Optional values will return Some and if any value has None will return None
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match{
      case Nil => Some(List[A]())
      case h :: t => h flatMap(x => sequence(t).map(x :: _))
    }
  }

  //Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
   a match{
     case Nil => Some(List[B]())
     case h :: t => f(h) flatMap(a => traverse(t)(f).map(a :: _))
   }
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
}

sealed trait Either[+E , +A] {

  //Example 4.6
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(a) => Left(a)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E,  B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
 for {
   a <- this
   bb <- b
 } yield f(a, bb)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  //Book example
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if(xs.isEmpty) Left("empty")
    else
      Right(xs.sum / xs.length)
  }
  //Book example
  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x/ y)
    catch{case e: Exception => Left(e)}
  }

  //Exercise 4.7 sequence and either for Either
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
   traverse(es)(x => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
     }
  }

  //Book Example
  case class  Person(name: Name, age: Age)
  sealed class Name(value: String)
  sealed class Age(value: Int)

  def mkName(name: String): Either[String, Name] = {
    if(name == null || name == "") Left("name is not valid")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if(age < 0) Left("age is out of range")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[String, Person] ={
    mkName(name).map2(mkAge(age))(Person(_, _))
  }


  //Exercise 4.8 above function will return only 1 error to return multiple errors we can introduce a new data type

  trait NewEither[+E, +B]
  case class Errors[+E](get: Seq[E]) extends NewEither[E, Nothing]
  case class Success[+B](get: B) extends NewEither[Nothing,B]


}





