package chapter_five

class Exercises {

  //Book Example
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A) = {
    if(cond) onTrue
    else onFalse
  }

}
