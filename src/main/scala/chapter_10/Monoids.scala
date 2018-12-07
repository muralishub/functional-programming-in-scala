package chapter_10

object Monoids {

  //Laws of associativity and Identity are collectively called monoid laws

  //what is laws of identity.
  // In programming we do things like concatinating strings , where empty string here is identity, for addition its o for multiplication its 1, for a list it can be empty list

  //what is associativity
  //if I am adding no (a + b) + c =  a + (b + c) or even a + b + c  if this happens its associativity
// the term for this kind of algebra is called monoid

  //what is a monoid?
  //its simply a type togeather with monoid operations and a set of laws


  // Definition of monoid
  //Its simply a type A and an implementation of Monoid[A] that satisfies the laws. a monoid is a type
  //togeather with a binary operation over that type satisfying associativity and having an identity element Zero


  //eg: String monoid
//Book Example
  val stringMonoid = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def zero: List[A] = List[A]()

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }


  //Exercise 10.1 give monoid instances of these
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 +  a2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }


  //Exercise 10.2 give a Monoid instance for combining Option values.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None
    override def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
  }

//Exercise 10.3 write a monoid for endofunctions
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  override def zero: A => A = (a: A) => a

  override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
}




trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}