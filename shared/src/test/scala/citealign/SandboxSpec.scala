package edu.furman.classics.citealign
import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._


/**
*/
class SandboxSpec extends FlatSpec {

  "The sandbox" should "group sequences" in {

    val tokens:List[Int] = List(1,2,3,10,20,21,22,30,40,41,42,50)
    val desired:Vector[Vector[Int]] = Vector(Vector(1, 2, 3), Vector(10), Vector(20, 21, 22), Vector(30), Vector(40, 41, 42), Vector(50))

    val (acc, last) = tokens
        .foldLeft ((List[List[Int]](), List[Int]())) ((a,b) => 
            if ( a._2.size == 0  )  {
                (a._1 :+ a._2, List(b)) 
            }
            else if ( (a._2.last + 1) != b  ) {
                (a._1 :+ a._2, List(b))
            }
            else {
                (a._1, a._2 :+ b)
            }
        )

    val answerAsLists:List[List[Int]] = (acc :+ last).tail
    val answer:Vector[Vector[Int]] = answerAsLists.map(_.toVector).toVector

    assert(answer == desired)

}

}
