package chapter_1
import scala.collection.immutable

class Cafe {

class TT

  def buyCoffee(cc: CreditCard): Coffee  = {
    val cup = new Coffee()
    cc.charge(cup.price) // this is side effect as it charges the credit card. And this method is interacting with
    //the outside world by calling the credit card company via a service and something happens with that call and this
    // function is returning the Coffee . hence is a side effect. there is also lack of testability here because of
    // interaction with CreditCard company
    cup
  }

  def buyCoffee(cc: CreditCard, p: Payment) = {
    val cup = new Coffee()
    p.charge(cc, cup.price)
    cup
  }


  def buyCoffeeF(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))

  }


  def buyCoffeesF(cc: CreditCard, no: Int): (List[Coffee], Charge) = {
   val purchases =  List.fill(no)(buyCoffeeF(cc))
    val (coffes, charges: Seq[Charge]) = purchases.unzip
    (coffes, charges.reduce((x, y) => x.combine(y)))
  }


}


class Coffee {
  val price = 2
}

class CreditCard {
  def charge(c: Int) = 2
}

class Payment {
  def charge(cc: CreditCard, price: Int) = ???
}

case class Charge(cc: CreditCard, amount:Double) {

  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
else
   throw new Exception("cant combine charges to different chards")

}

object main extends App {

  val c = (new Cafe)
  


  private val strings = c.getClass.getName.split('.').reverse.head.toLowerCase
  println(strings)


}