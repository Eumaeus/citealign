package edu.furman.classics.citealign
import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import scala.io.Source


/**
*/
class CiteAlignmentManagerSpec extends FlatSpec {

  def loadLibrary(fp:String = "cex/test_alignments.cex"):CiteLibrary = {
    val library = CiteLibrary(Source.fromFile(fp).getLines.mkString("\n"),"#",",")
    library
  }

  "A CiteAlignmentManager" should "build" in {
    val lib:CiteLibrary = loadLibrary()
    assert(lib.textRepository.get.corpus.size == 2003)
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    assert(cam.isValid)
  }

  it should "fail to buld gracefully if there are no alignments in a library" in {
    val lib:CiteLibrary = loadLibrary("cex/no_alignments.cex")
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    assert(cam.isValid == false)

  }

  it should "include all records of the recording CITE Object" in pending

  it should "list alignment collections present in a CiteLibrary" in pending

  it should "list alignments filtered by Cite2Urn" in pending

  it should "list texts participating in an alignment" in pending

  it should "list texts participating in a vector of alignments" in pending

  it should "list alignments for a text" in pending

  it should "return a Vector[CtsUrn] for an alignment" in pending

  it should "return a Corpus for a vector of alignments" in pending

  it should "export a corpus and alignments as CEX" in pending


}
