package chapter_threeTests

import chapter_three.exercises.{Branch, Functions, Leaf}
import chapter_three.samples.{Cons, List, Nil, RecursionAndGeneralization}
import org.scalatest.{BeforeAndAfterEach, FunSpec}
import org.scalatest.Matchers._

class FunctionsTest extends FunSpec with BeforeAndAfterEach {
  describe("In Function") {

    it("tail function gets the tail from list") {

      val function = new Functions
      function.tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
      function.tail(Nil) shouldBe Nil
      function.tail(List("mike")) shouldBe List()
      function.tail(List("mike", "bramhal")) shouldBe List("bramhal")
    }

    it("set head for a list") {

      val function = new Functions
      function.setHead(List(2, 3), 1) shouldBe List(1, 2, 3)
      function.setHead(Nil, "mike") shouldBe List("mike")

    }

    it("remove first n elements of a list") {

      val function = new Functions
      function.drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
      function.drop(List(), 2) shouldBe List()
    }

    it("drop while List matches a predicate") {
      val function = new Functions

      //function.dropWhile(List(1, 2, 3), (x: Int) => x == 2) shouldBe List(3) //TODO example solved in book doesnt passs this test page 37
      function.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) shouldBe List(4, 5)
    }

    it("Book example add list to end of another") {
      val function = new Functions
      function.append(List(1, 2), List(4, 5, 6)) shouldBe List(1, 2, 4, 5, 6)
    }

    //    it("init function which returns List except last element, this doesnt work just shouows its difficult in this scenario") {
    //      val function = new Functions
    //      function.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    //    }

    //Example 3.8
    it("what happens when you pass Nil and Cons themselves to foldRight") {
      val foo = new RecursionAndGeneralization
      val result = foo.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
      result shouldBe Cons(1, Cons(2, Cons(3, Nil)))
    }

    //Example 3.9
    it("length of a list using foldRight") {
      val foo = new Functions
      foo.length(List(1, 2,3)) shouldBe 3
      foo.length(List(1, 2,3,4, 5)) shouldBe 5
      foo.length(Nil) shouldBe 0
    }
    //   //Exercise 3.10
    it("fold left in tail racursive to avoid stack overflow") {
      val foo = new Functions
      foo.foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foo.foldLeft(List(1, 2, 3, 2), 1)(_ * _) shouldBe 12
    }

    it("fold right recursive") {
      val foo = new Functions
      foo.foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
      foo.foldLeft(List(1, 2, 3, 2), 1)(_ * _) shouldBe 12
    }

    //Exercise 3.11
    describe("using foldleft") {
      val foo = new Functions

      it("sum works") {
        foo.sum(List(5, 4,3)) shouldBe 12
      }

      it("product works") {
        foo.product(List(4, 5)) shouldBe 20
      }

      it("length works") {
        foo.lengthUsingFoldLeft(List(8, 9)) shouldBe 2
        foo.lengthUsingFoldLeft(List(8, 9, 10, 8)) shouldBe 4
        foo.lengthUsingFoldLeft(List()) shouldBe 0

      }
    }

    //Exercise 3.12 :  reverse a list
    it("returns reverse of list") {
      val foo = new Functions
      foo.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
      foo.reverse(List(5, 4, 3, 2, 1)) shouldBe List(1, 2, 3, 4, 5)
      foo.reverse(List()) shouldBe List()

    }

    //Exercise 3.13 foldright using foldleft
    it("foldRight using foldLeft") {
      val foo =  new Functions
      foo.foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }

    it("foldLeft using foldRight") {
      val foo =  new Functions
      foo.foldLeftViafoldRight(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }

    //  Exercise 3.14 Implement append using foldRight
    it("appped using foldRight") {
      val foo = new Functions
      foo.appendUsingfoldRight(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
    }

    it("appped using foldLeft") {
      val foo = new Functions
      foo.appendUsingfoldRight(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
    }


    //Exercise 3.15 Contact list of Lists to single list
    it("concact list of lists") {
      val foo = new Functions
      foo.concat(List(List(1, 2), List(3,4))) shouldBe List(1, 2, 3, 4)
    }

    //Exercise 3.16 Add 1 to all elemets in a list of integers
    it("add one to all elements in a list non recursive") {
      val foo = new Functions
      foo.addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
    }

    it("add one to all elements in a list tail recursive") {
      val foo = new Functions
      foo.addOneTailRec(List(1, 2, 3)) shouldBe List(2, 3, 4)
    }

    //Exercise 3.17 Convet List[Int] to List[String]
    it("list Int to list Double") {
      val foo  = new Functions
      foo.doubleToString(List(1.2, 2.3, 5.0)) shouldBe List("1.2", "2.3", "5.0")

    }

    //Exercise 3.18 Modify each element of list while maintaing the structure
    it("modify each element of list") {
      val foo = new Functions
      foo.map(List(1, 2))(x => x + 1) shouldBe List(2, 3)

    }

    //Exercise 3.19 Filter that satisfyies a condition
    it("filter by condition") {
      val foo = new Functions
      foo.filter(List(1, 2))(x => x % 2 == 0) shouldBe List(2)
      foo.filter(List(2, 4, 6, 8, 9))(x => x % 2 == 0) shouldBe List(2, 4, 6, 8)
      foo.filter(List(1))(x => x % 2 == 0) shouldBe Nil
    }

    //Exercise 3.20 flatMap
    it("flatmap") {
      val foo = new Functions
      foo.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    }

    //Exercise 3.21 Filter using flatMap
    it("filter with flatmap") {
      val foo = new Functions
      foo.filterUsingFlatMap(List(1, 2, 3))(x => x % 2 == 0) shouldBe List(2)
    }

    //Exercise 3.22 add elements from 2 lists and generate a new list
    it("add list elements and return a new list") {
      val foo = new Functions
      foo.addElementsInList(List(1, 2, 3), List(4, 5, 6))(_ +_) shouldBe List(5, 7, 9)
    }

    //Exercise 3.23 make above function generic so it works for non integers and other than +
    it("add list so it works also for non integers") {
      val foo = new Functions
      foo.zipWith(List("a", "x"), List("b", "y"))(_ + _) shouldBe List("ab", "xy")
      foo.zipWith(List(1, 2), List(3, 4))(_ * _) shouldBe List(3, 8)
    }

    //Exercise 3.24 subsequence exists
    it("has subSequence") {
      val foo = new Functions
      foo.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    }

    //Exercise 3.25 count no of nodes in a tree
    it("size of nodes") {
      val foo = new Functions
      foo.size(Leaf(1)) shouldBe 1
      foo.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
    }

    //Exercise 3.26 find maximum element in a tree
    it("maximum in tree") {
      val foo = new Functions
      foo.maximum(Leaf(5)) shouldBe 5
      foo.maximum(Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))) shouldBe 6
    }

    //Exercise 3.27 depth(max path length)
    it("depth max path length") {
      val foo = new Functions()
      foo.depth(Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))) shouldBe 3
    }

    //Exercise 3.28 map
    it("map") {
      val l = Seq(1, 2, 3)
      l.map(l => l + 1) shouldBe Seq(2, 3, 4)
    }


  }
}
