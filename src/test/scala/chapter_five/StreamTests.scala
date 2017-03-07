package chapter_five

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class StreamTests[A]  extends FunSpec with Stream[A]{

  it("cons") {
    val s = Stream
    s.cons(1, Stream(1, 2)).toList shouldBe Stream(1, 1, 2).toList
  }

  it("apply") {
    val s = Stream
    s.apply(1, 2, 3).toList shouldBe Stream(1,2, 3).toList
  }

  //Exercise 5.1 convert stream to list
  it("toList") {
    Stream(1, 2).toList shouldBe List(1, 2)
  }
  //Exercise 5.2 take and drop
  it("take") {
    Stream(1,2,3,4).take(2).toList shouldBe List(1, 2)
  }

  it("drop") {
    Stream(1,2, 3, 4).drop(2).toList shouldBe List(3, 4)
  }

  //Exercise 5.3 take while that matches a pridicate
  it("takeWhile") {

    Stream(3, 7, 2).takeWhile(_ % 2 == 1).toList shouldBe List(3, 7)

  }
  //Exercise 5.4
  it("forAll") {
    Stream(2, 4, 7).forAll(_ % 2 == 1) shouldBe false
    Stream(3, 5, 7).forAll(_ % 2 == 1) shouldBe true
  }

  //Exercise 5.5 takewhile
  it("takeWhile using foldRight") {
    Stream(3, 7, 2).takeWhileUsingFoldRight(_ % 2 == 1).toList shouldBe Stream(3, 7).toList
    Stream(2).takeWhileUsingFoldRight(_ % 2 == 1).toList shouldBe Stream().toList
  }

  //Exercise 5.6 headOption using foldRight
  it("headOption") {
    Stream(1, 2).headOption shouldBe Some(1)
  }

  //Exercise 5.7 map, filter
  it("map") {
    Stream(1, 2).map(x => x + 1).toList shouldBe List(2, 3)
  }

  it("filter") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  it("append") {
    Stream(1, 2, 3).append(Stream(4)).toList shouldBe List(1, 2, 3, 4)
  }

  it("flatMap") {
    Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList shouldBe Stream(1, 1, 2, 2, 3, 3).toList
  }
  //Exercise 5.9
  it("from increment +1 starting fro n")  {
    from(1).take(3).toList shouldBe List(1, 2, 3)
  }

  //Exercise 5.10
  it("fibs") {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  //Exercise 5.11
  it("unfold") {
    unfold(1)((x) => Some(1, 1)).take(2).toList shouldBe List(1,1)
  }

  //Exercise 5.12
  it("fibs using unfold") {
    fibsUsingUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  it("from using unfold") {
    fromUsingUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  it("constant using unfold") {
    constantUsingUnfold(2).take(5).toList shouldBe List(2, 2, 2, 2, 2)
  }

  it("ones using unfold") {
    onesUsingUnfold.take(2).toList shouldBe List(1, 1)
  }

  //Exercise 5.13
  it("mapUsingUnfold") {
    Stream(1, 2).map(x => x + 1).toList shouldBe List(2, 3)
  }
  it("takeUsingUnfold") {
    Stream(1, 2, 3).take(2).toList shouldBe List(1, 2)
    Stream(1).take(2).toList shouldBe List(1)
    Stream().take(3).toList shouldBe List()
  }
  it("takeWhileUsingUnfold"){
    Stream(2, 4, 6, 9).takeWhileUsingUnfold(_ % 2 == 0).toList shouldBe List(2, 4, 6)
    Stream(5, 6).takeWhileUsingUnfold(_ % 2 == 0).toList shouldBe List()

  }
  it("zipWithUsingUnfold") {
    Stream(1, 2, 3).zipWith(Stream(3, 4))((x, y) => x + y).toList shouldBe List(4, 6)
  }
  it("zipAll") {
    Stream(1, 2, 3).zipAll(Stream(3, 4)).toList shouldBe List((Some(1), Some(3)), (Some(2), Some(4)), (Some(3), None))
  }
  it("startsWith") {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) shouldBe false
    Stream(1, 2, 3).startsWith(Stream(2, 3)) shouldBe false
  }

  it("tails") {
    Stream(1,2,3).tails shouldBe Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  }

}



