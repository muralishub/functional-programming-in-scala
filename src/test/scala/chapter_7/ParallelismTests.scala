package chapter_7

import org.scalatest.FunSpec
import org.scalatest.Matchers._




case class GTModel(name: String)

class ParallelismTests extends FunSpec{

  val c = "test"


  it("test") {


    val test = Seq(GTModel("et")) ++ c match{

      case c: Seq[GTModel] => Seq(GTModel("gg"))
      case _ => Seq(GTModel("gg"))

    }

    val p =  new Parallelism
    p.sum(IndexedSeq[Int](1,2,3,4)) shouldBe 10
  }



}
