package chapeter.two.exercises

/**
  * Created by mraju on 10/11/16.
  */
object PolymorphicFunction extends App{


  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if(n >= ss.length) -1
      else if(n == key)n
      else
        loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](as: Array[A],p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if(n >= as.length) -1
      else if(n == p(as(n)))n
      else
        loop(n + 1)
    }
    loop(0)
  }

}
