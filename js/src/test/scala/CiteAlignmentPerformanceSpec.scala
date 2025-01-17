package edu.furman.classics.citealign
import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import scala.scalajs.js
import scala.scalajs.js._
import scala.io.Source


/**
*/
class CiteAlignmentPerformanceSpec extends FlatSpec {

  // Utility methods so we don't have to have many
  // different CEX strings in this test doc

  def loadLibrary(fp:String = goodCex):CiteLibrary = {
    val library = CiteLibrary(fp,"#",",")
    library
  }

  def removeLinesFromCex(cex:String, lineNum:Int):String = {
    removeLinesFromCex(cex, Vector(lineNum))
  }

  def removeLinesFromCex(cex:String, lineNums:Vector[Int]):String = {
    val lines:Vector[(String,Int)] = cex.split("\n").zipWithIndex.toVector
    val indices:Vector[Int] = lineNums.map(_ - 1)
    lines.filter(l => (indices.contains(l._2) == false)).map(_._1).mkString("\n")
  }

  def editLinesInCex(cex:String, lineNum:Int, replacement:String) = {
    val lines:Vector[(String,Int)] = cex.split("\n").zipWithIndex.toVector
    val index:Int = lineNum - 1
    val before:Vector[String] = lines.filter(l => l._2 < index).map(_._1)
    val after:Vector[String] = lines.filter(l => l._2 > index).map(_._1)
    val replaceVec:Vector[String] = Vector(replacement)
    val newCex = (before ++ replaceVec ++ after) .mkString("\n")
    newCex
  }


  // Actual tests

  "A CiteAlignmentManager" should "build" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    assert(cam.isValid)
  }

  it should "return a vector of urns to collections that record alignments" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val colls:Vector[Cite2Urn] = cam.alignmentCollections
    assert( colls.size == 1 )
    assert( colls.contains( Cite2Urn("urn:cite2:ducat:alignments.temp:")))
  }

  it should "return a vector of all aligment-objects in a library" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignments:Vector[CiteObject] = cam.alignments
    assert (alignments.size == 6 )
  }

  it should "return a vector of URNs for all aligment-objects in a library" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrns:Vector[Cite2Urn] = cam.alignmentUrns
    assert (alignmentUrns.size == 6 )
    alignmentUrns(0).asInstanceOf[Urn] match {
      case CtsUrn(_) => assert (false)
      case Cite2Urn(_) => assert (true)
      case _ => assert(false)
    }
  }

  it should "return a vector of all aligment-objects in a library filtered by Cite2Urn" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignments:Vector[CiteObject] = cam.alignments(u)
    assert (alignments.size == 6 )
  }

  it should "return a vector of URNs for all aligment-objects in a library filtered by Cite2Urn" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrns:Vector[Cite2Urn] = cam.alignmentUrns(u)
    assert (alignmentUrns.size == 1 )
    alignmentUrns(0).asInstanceOf[Urn] match {
      case CtsUrn(_) => assert (false)
      case Cite2Urn(_) => assert (true)
      case _ => assert(false)
    }
  }

  it should "return a Vector of CtsUrns of texts participating in an alignment defined by an object-level URN" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"))
    assert( cam.textsAligned(u).toSet == textsPresent.toSet)
  }

  it should "return a Vector of CtsUrns of texts participating in an alignment defined by a collection-level URN" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"))
    assert( cam.textsAligned(u).toSet == textsPresent.toSet )
  }

  it should "list texts participating in a vector of alignments" in {
    val u:Vector[Cite2Urn] = Vector(
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1")
    )
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"))
    assert( cam.textsAligned(u).toSet == textsPresent.toSet )

  }

  it should "list texts participating in all recorded alignments" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:")
    )
    assert( cam.textsAligned.toSet == textsPresent.toSet )
  }
  /*

  it should "construct a CiteAlignment object from a Cite2Urn" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrn:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1")
    val passages:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0-8.22.6"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3-8.22.12")
    )
    val alignment:Vector[CiteAlignment] = cam.getAlignments(alignmentUrn)
    assert( alignment.size == 1)
    assert( alignment.head.urn == alignmentUrn)
    assert( alignment.head.passages == passages)
  }
*/

  it should "reorder a Set[CtsUrn] into a Vector[CtsUrn] according to document order from a corpus" in {
      val lib:CiteLibrary = loadLibrary()
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
      val shuffle:Set[CtsUrn] = Set(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7-8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2-8.22.5")
      )
      val sorted:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12")
      )
      assert( cam.sortPassages(shuffle) == sorted)
  }

  it should "reorder a Vector[CtsUrn] into a Vector[CtsUrn] according to document order from a corpus" in {
      val lib:CiteLibrary = loadLibrary()
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
      val shuffle:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7-8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2-8.22.5")
      )
      val sorted:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12")
      )
      assert( cam.sortPassages(shuffle) == sorted)
  }


  it should "compress a Vector[CtsUrn] into ranges where possible" in {
      val lib:CiteLibrary = loadLibrary()
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
      val shuffle:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.29"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.9")
      )
      val expected:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1-8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8-8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.29")
      )
      val sorted:Vector[CtsUrn] = cam.sortPassages(shuffle)
      val compressed:Vector[CtsUrn] = cam.compressReff(sorted)
      //println(s"""\n\n-----\ncompressed\n----\n${compressed.mkString("\n")}\n\n----""")
      assert(expected == compressed)

  }

  it should "compress a Vector[CtsUrn] into ranges where possible even when there are URNs of different works" in {
      val lib:CiteLibrary = loadLibrary()
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
      val shuffle:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.2"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10")
      )
      val expected1:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1-8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1-8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10")
      )
      val expected2:Vector[CtsUrn] = Vector(
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1-8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1-8.22.3"),
        CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10")
      )
      val sorted:Vector[CtsUrn] = cam.sortPassages(shuffle)
      val compressed:Vector[CtsUrn] = cam.compressReff(sorted)
      assert( (expected1 == compressed) | (expected2 == compressed))

  }

  it should "list alignments for a text" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentsPresent:Set[Cite2Urn] = Set(
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_2"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_3"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_4"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_5")
    )
    val alignments:Set[CiteAlignment] = cam.alignmentsForText(textUrn)
    val aligmentUrns:Set[Cite2Urn] = alignments.map(_.urn)
    assert( aligmentUrns == alignmentsPresent )
  }
 /* 

  it should "list alignments for a notional text" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:fufolio:pope.iliad:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentsPresent:Set[Cite2Urn] = Set(
      Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:3"),
      Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:4"),
      Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:5"),
      Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:6")
    )
    val alignments:Set[CiteAlignment] = cam.alignmentsForText(textUrn)
    val aligmentUrns:Set[Cite2Urn] = alignments.map(_.urn)
    assert( aligmentUrns == alignmentsPresent )
  }
  */

  it should "list alignments for a passage" in {
    val passage:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)

    val expectedSet = cam.getAlignments(Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1")).toSet
    val als:Set[CiteAlignment] = cam.alignmentsForText(passage)
    assert( als == expectedSet)

  }

  it should "list alignments for a range" in {
var time0 = new js.Date().getTime()
    val passage:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.0-8.22.1")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val expectedColls:Vector[Cite2Urn] = Vector(
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0"),Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1")
    )
    val expectedSet = cam.getAlignments(expectedColls).toSet
    val als:Set[CiteAlignment] = cam.alignmentsForText(passage)
var timeEnd = new js.Date().getTime()
println(s"Ran alignmentsForText in ${(timeEnd - time0) / 1000 } seconds")
    assert( als == expectedSet)
  }

  /*

  it should "list alignments for a vector of CtsUrns" in {
    val passages:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10"),
      CtsUrn("urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4")
    )
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val expectedColls:Vector[Cite2Urn] = Vector(
      Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1"),Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:4"),Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:5")
    )
    val expectedSet = cam.getAlignments(expectedColls).toSet
    val als:Set[CiteAlignment] = cam.alignmentsForText(passages)
    assert( als == expectedSet)
  }

  it should "return a Vector[CtsUrn] for an alignment, with ranges expanded" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val expectedSet = cam.getAlignments(Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1")).toSet
    assert(expectedSet.size == 1)
    val thisAlignment:CiteAlignment = expectedSet.head
    val expectedVector:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.4"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.5"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.6"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12")
    )
    val returnVec:Vector[CtsUrn] = cam.passagesForAlignment(thisAlignment)
    assert( returnVec == expectedVector)
  }
*/
  

  it should "return a Corpus for a vector of alignments" in {
var time0 = new js.Date().getTime()
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignVec:Vector[Cite2Urn] = Vector(Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_2"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_3"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_4"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_5"))
    val testCorp:edu.holycross.shot.ohco2.Corpus = cam.corpusForAlignments(alignVec)
var timeEnd = new js.Date().getTime()
println(s"Ran (false) in ${(timeEnd - time0) / 1000 } seconds")
    assert(testCorp.size == 22)
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc:")).nodes(0).urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0"))
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng:")).last.urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.13"))
  }

  it should "return a Corpus for a vector of alignments, expanded to the containing element" in {
var time0 = new js.Date().getTime()
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignVec:Vector[Cite2Urn] = Vector(Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_0"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_2"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_3"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_4"), Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_5"))
    val testCorp:edu.holycross.shot.ohco2.Corpus = cam.corpusForAlignments(alignVec, true)
var timeEnd = new js.Date().getTime()
println(s"Ran (true) in ${(timeEnd - time0) / 1000 } seconds")
    assert(testCorp.size == 322)
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc:")).nodes(0).urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0"))
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng:")).last.urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.176"))
  }
  

/*

  it should "export a corpus and alignments as CEX" in pending

  it should "have a utility function for grouping sequences of integer, as lists" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:fufolio:pope.iliad:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val tokens:List[Int] = List(1,2,3,10,20,21,22,30,40,41,42,50)
    val desired:List[List[Int]] = List(List(1, 2, 3), List(10), List(20, 21, 22), List(30), List(40, 41, 42), List(50))
    val answer:List[List[Int]] = cam.groupSequences(tokens)
    assert(answer == desired)
  } 

  it should "have a utility function for grouping sequences of integer, as vectors" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:fufolio:pope.iliad:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val tokens:Vector[Int] = Vector(1,2,3,10,20,21,22,30,40,41,42,50)
    val desired:Vector[Vector[Int]] = Vector(Vector(1, 2, 3), Vector(10), Vector(20, 21, 22), Vector(30), Vector(40, 41, 42), Vector(50))
    val answer:Vector[Vector[Int]] = cam.groupSequences(tokens)
    assert(answer == desired)
  } 
  */

val goodCex:String = """#!cexversion
3.0

#!citelibrary
name#CITE Library generated by the Ducat application, Fri Mar 01 2019 18:09:14 GMT+0100 (CET)
urn#urn:cite2:cex:ducatauto.20192:18_9_14_991
license#CC Share Alike.
        

// URL for state at export:
// file:///Users/cblackwell/Dropbox/CITE/scala/reader-app/target/scala-2.12/classes/index-dev.html?urn=urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22&urn=urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22&urn=


#!ctscatalog
urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:#book/section/token#Herodotus#Histories#Greek, Godley, ed.#tokenized#true#grc
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:#book/section/token#Herodotus#Histories#English, trans. Godley#tokenized, no punctuation#true#eng


//Herodotus, Histories (Greek, Godley, ed.: tokenized)
#!ctsdata
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0#Ἀθηναίων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2#νέας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3#τὰς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4#ἄριστα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5#πλεούσας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6#ἐπιλεξάμενος
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.7#Θεμιστοκλέης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8#ἐπορεύετο
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.9#περὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.11#πότιμα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.12#ὕδατα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.14#ἐντάμνων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.15#ἐν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.16#τοῖσι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.17#λίθοισι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.18#γράμματα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.20#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.21#Ἴωνες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.22#ἐπελθόντες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.23#τῇ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.24#ὑστεραίῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.25#ἡμέρῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.26#ἐπὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.27#τὸ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.28#Ἀρτεμίσιον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.29#ἐπελέξαντο
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.31#Τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.32#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.33#γράμματα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.34#τάδε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.35#ἔλεγε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.38#Ἄνδρες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.39#Ἴωνες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.41#οὐ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.42#ποιέετε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.43#δίκαια
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.44#ἐπὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.45#τοὺς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.46#πατέρας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.47#στρατευόμενοι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.48#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.49#τὴν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.50#Ἑλλάδα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.51#καταδουλούμενοι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.53#Ἀλλὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.54#μάλιστα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.55#μὲν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.56#πρὸς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.57#ἡμέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.58#γίνεσθε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.60#εἰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.61#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.62#ὑμῖν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.63#ἐστι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.64#τοῦτο
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.65#μὴ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.66#δυνατὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.67#ποιῆσαι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.69#ὑμεῖς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.70#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.71#ἔτι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.72#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.73#νῦν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.74#ἐκ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.75#τοῦ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.76#μέσου
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.77#ἡμῖν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.78#ἕζεσθε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.79#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.80#αὐτοὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.81#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.82#τῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.83#Καρῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.84#δέεσθε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.85#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.86#αὐτὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.87#ὑμῖν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.88#ποιέειν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.90#εἰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.91#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.92#μηδέτερον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.93#τούτων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.94#οἷόν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.95#τε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.96#γίνεσθαι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.98#ἀλλ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.99#ὑπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.100#ἀναγκαίης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.101#μέζονος
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.102#κατέζευχθε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.103#ἢ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.104#ὥστε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.105#ἀπίστασθαι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.107#ὑμεῖς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.108#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.109#ἐν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.110#τῷ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.111#ἔργῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.113#ἐπεὰν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.114#συμμίσγωμεν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.116#ἐθελοκακέετε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.118#μεμνημένοι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.119#ὅτι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.120#ἀπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.121#ἡμέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.122#γεγόνατε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.123#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.124#ὅτι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.125#ἀρχῆθεν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.126#ἡ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.127#ἔχθρη
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.128#πρὸς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.129#τὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.130#βάρβαρον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.131#ἀπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.132#ὑμέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.133#ἡμῖν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.134#γέγονε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.137#Θεμιστοκλέης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.138#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.139#ταῦτα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.140#ἔγραφε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.142#δοκέειν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.143#ἐμοί
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.145#ἐπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.146#ἀμφότερα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.147#νοέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.149#ἵνα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.150#ἢ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.151#λαθόντα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.152#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.153#γράμματα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.154#βασιλέα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.155#Ἴωνας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.156#ποιήσῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.157#μεταβαλεῖν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.158#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.159#γενέσθαι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.160#πρὸς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.161#ἑωυτῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.163#ἢ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.164#ἐπείτε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.165#ἂν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.166#ἀνενειχθῇ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.167#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.168#διαβληθῇ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.169#πρὸς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.170#Ξέρξην
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.172#ἀπίστους
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.173#ποιήσῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.174#τοὺς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.175#Ἴωνας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.176#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.177#τῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.178#ναυμαχιέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.179#αὐτοὺς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.180#ἀπόσχῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.0#Θεμιστοκλέης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.1#μὲν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.2#ταῦτα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.3#ἐνέγραψε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.5#Τοῖσι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.6#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.7#βαρβάροισι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.8#αὐτίκα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.9#μετὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.10#ταῦτα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.11#πλοίῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.12#ἦλθε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.13#ἀνὴρ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.14#Ἱστιαιεὺς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.15#ἀγγέλλων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.16#τὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.17#δρησμὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.18#τὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.19#ἀπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.20#Ἀρτεμισίου
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.21#τῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.22#Ἑλλήνων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.24#Οἱ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.25#δ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.26#ὑπ᾽
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.27#ἀπιστίης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.28#τὸν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.29#μὲν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.30#ἀγγέλλοντα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.31#εἶχον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.32#ἐν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.33#φυλακῇ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.35#νέας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.36#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.37#ταχέας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.38#ἀπέστειλαν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.39#προκατοψομένας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.41#Ἀπαγγειλάντων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.42#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.43#τούτων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.44#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.45#ἦν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.47#οὕτω
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.48#δὴ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.49#ἅμα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.50#ἡλίῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.51#σκιδναμένῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.52#ἅπασα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.53#ἡ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.54#στρατιὴ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.55#ἔπλεε
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.56#ἁλὴς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.57#ἐπὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.58#τὸ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.59#Ἀρτεμίσιον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.61#Ἐπισχόντες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.62#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.63#ἐν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.64#τούτῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.65#τῷ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.66#χώρῳ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.67#μέχρι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.68#μέσου
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.69#ἡμέρης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.71#τὸ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.72#ἀπὸ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.73#τούτου
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.74#ἔπλεον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.75#ἐς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.76#Ἱστιαίην
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.78#ἀπικόμενοι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.79#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.80#τὴν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.81#πόλιν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.82#ἔσχον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.83#τῶν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.84#Ἱστιαιέων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.86#καὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.87#τῆς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.88#Ἐλλοπίης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.89#μοίρης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.91#γῆς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.92#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.93#τῆς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.94#Ἱστιαιώτιδος
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.96#τὰς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.97#παραθαλασσίας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.98#κώμας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.99#πάσας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.23.100#ἐπέδραμον

//Herodotus, Histories (English, trans. Godley: tokenized, no punctuation)
#!ctsdata
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.0#Themistocles
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1#however
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.2#selected
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3#those
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.4#ships
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.5#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.6#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7#Athenians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8#which
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9#sailed
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10#best
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.13#went
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.14#round
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.15#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.16#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.17#springs
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.18#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.19#drinking-water
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.21#cutting
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.22#inscriptions
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.23#on
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.24#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.25#stones
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.26#there
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.28#which
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.29#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.30#Ionians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.31#read
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.32#when
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.33#they
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.34#came
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.35#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.36#Artemision
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.37#on
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.38#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.39#following
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.40#day
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.42#These
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.43#inscriptions
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.44#ran
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.45#thus:
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.46#Ionians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.48#ye
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.49#act
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.50#not
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.51#rightly
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.52#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.53#making
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.54#expedition
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.55#against
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.56#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.57#fathers
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.58#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.59#your
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.60#race
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.61#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.62#endeavouring
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.63#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.64#enslave
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.65#Hellas
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.67#Best
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.68#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.69#all
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.70#were
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.71#it
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.72#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.73#ye
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.74#should
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.75#come
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.76#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.77#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.78#on
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.79#our
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.80#side
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.82#but
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.83#if
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.84#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.85#may
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.86#not
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.87#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.88#done
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.89#by
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.90#you
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.92#stand
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.93#aside
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.94#even
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.95#now
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.96#from
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.97#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.98#combat
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.99#against
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.100#us
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.101#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.102#ask
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.103#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.104#Carians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.105#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.106#do
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.107#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.108#same
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.109#as
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.110#ye
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.112#If
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.113#however
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.114#neither
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.115#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.116#these
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.117#two
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.118#things
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.119#is
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.120#possible
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.121#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.122#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.123#done
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.125#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.126#ye
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.127#are
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.128#bound
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.129#down
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.130#by
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.131#too
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.132#strong
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.133#compulsion
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.134#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.135#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.136#able
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.137#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.138#make
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.139#revolt
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.141#then
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.142#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.143#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.144#action
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.146#when
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.147#we
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.148#engage
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.149#battle
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.151#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.152#purposely
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.153#slack
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.155#remember
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.156#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.157#ye
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.158#are
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.159#descended
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.160#from
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.161#us
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.162#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.163#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.164#our
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.165#quarrel
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.166#with
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.167#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.168#Barbarian
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.169#took
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.170#its
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.171#rise
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.172#at
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.173#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.174#first
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.175#from
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.176#you
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.0#Themistocles
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.1#wrote
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.2#thus
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.4#having
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.6#as
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.7#I
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.8#suppose
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.10#two
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.11#things
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.12#together
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.13#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.14#his
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.15#mind
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.17#namely
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.18#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.19#either
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.20#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.21#inscriptions
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.22#might
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.23#elude
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.24#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.25#notice
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.26#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.27#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.28#king
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.29#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.30#cause
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.31#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.32#Ionians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.33#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.34#change
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.35#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.36#come
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.37#over
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.38#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.39#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.40#side
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.41#on
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.42#which
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.43#he
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.44#was
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.46#or
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.47#that
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.48#having
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.49#been
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.50#reported
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.51#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.52#denounced
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.53#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.54#Xerxes
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.55#they
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.56#might
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.57#cause
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.58#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.59#Ionians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.60#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.61#be
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.62#distrusted
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.63#by
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.64#him
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.66#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.67#so
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.68#he
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.69#might
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.70#keep
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.71#them
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.72#apart
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.73#from
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.74#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.75#sea-
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.76#fights
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.78#Themistocles
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.79#then
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.80#had
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.81#set
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.82#these
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.83#inscriptions:
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.84#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.85#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.86#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.87#Barbarians
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.88#there
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.89#came
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.90#immediately
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.91#after
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.92#these
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.93#things
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.94#a
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.95#man
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.96#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.97#Histaia
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.98#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.99#a
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.100#boat
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.101#bringing
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.102#word
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.103#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.104#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.105#retreat
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.106#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.107#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.108#Hellenes
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.109#from
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.110#Artemision
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.112#They
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.113#however
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.115#not
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.116#believing
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.117#it
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.119#kept
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.120#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.121#messenger
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.122#under
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.123#guard
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.124#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.125#sent
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.126#swift-sailing
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.127#ships
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.128#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.129#look
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.130#on
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.131#before
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.133#Then
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.134#these
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.135#having
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.136#reported
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.137#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.138#facts
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.140#at
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.141#last
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.142#as
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.143#daylight
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.144#was
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.145#spreading
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.146#over
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.147#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.148#sky
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.150#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.151#whole
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.152#armament
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.153#sailed
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.154#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.155#a
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.156#body
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.157#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.158#Artemision
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.160#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.161#having
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.162#stayed
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.163#at
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.164#this
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.165#place
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.166#till
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.167#mid-day
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.169#after
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.170#this
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.171#they
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.172#sailed
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.173#to
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.174#Histaia
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.176#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.177#there
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.178#arrived
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.179#they
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.180#took
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.181#possession
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.182#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.183#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.184#city
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.185#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.186#Histaia
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.187#and
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.188#overran
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.189#all
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.190#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.191#villages
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.192#which
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.193#lie
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.194#along
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.195#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.196#coast
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.197#in
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.198#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.199#region
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.200#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.201#Ellopia
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.203#which
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.204#is
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.205#the
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.206#land
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.207#of
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.23.208#Histaia

#!datamodels
Collection#Model#Label#Description
urn:cite2:ducat:alignments.temp:#urn:cite2:cite:datamodels.v1:alignment#Text Alignment Model#The CITE model for text alignment. See documentation at <https://eumaeus.github.io/citealign/>.

#!citecollections
URN#Description#Labelling property#Ordering property#License
urn:cite2:cite:datamodels.v1:#CITE data models#urn:cite2:cite:datamodels.v1.label:##Public domain
urn:cite2:cite:verbs.v1:#Collection of verbal relations#urn:cite2:cite:verbs.v1.label:##Public Domain

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:cite:datamodels.v1.urn:#Data model#Cite2Urn#
urn:cite2:cite:datamodels.v1.label:#Label#String#
urn:cite2:cite:datamodels.v1.description:#Description#String#

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:cite:verbs.v1.urn:#URN#Cite2Urn#
urn:cite2:cite:verbs.v1.label:#label#String#
urn:cite2:cite:verbs.v1.description:#description#String#


#!citedata
urn#label#description
urn:cite2:cite:verbs.v1:illustrates#illustrates#subject[Urn] comments on object[Urn]
urn:cite2:cite:verbs.v1:commentsOn#comments on#subject[Urn] comments on object[Urn]
urn:cite2:cite:verbs.v1:aligns#aligns#subject[CiteUrn] is an alignment that includes passage[CtsUrn]
urn:cite2:cite:verbs.v1:hasOnIt#has on it#subject[Urn] comments on object[Urn]



#!citecollections
URN#Description#Labelling property#Ordering property#License
urn:cite2:ducat:alignments.temp:#Citation Alignments#urn:cite2:ducat:alignments.temp.label:##CC-BY 3.0

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:ducat:alignments.temp.urn:#Alignment Record#Cite2Urn#
urn:cite2:ducat:alignments.temp.label:#Label#String#
urn:cite2:ducat:alignments.temp.description:#Description#String#
urn:cite2:ducat:alignments.temp.editor:#Editor#String#
urn:cite2:ducat:alignments.temp.date:#Date#String#

#!citedata
urn#label#description#editor#date
urn:cite2:ducat:alignments.temp:2019218_10_20_669_0#Alignment 0#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT
urn:cite2:ducat:alignments.temp:2019218_10_20_669_1#Alignment 1#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT
urn:cite2:ducat:alignments.temp:2019218_10_20_669_2#Alignment 2#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#Alignment 3#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT
urn:cite2:ducat:alignments.temp:2019218_10_20_669_4#Alignment 4#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT
urn:cite2:ducat:alignments.temp:2019218_10_20_669_5#Alignment 5#Textual Alignment created with the Ducat tool on: Fri Mar 01 2019 18:10:20 GMT+0100 (CET).#CWB#Fri, 01 Mar 2019 17:10:20 GMT

#!relations
urn:cite2:ducat:alignments.temp:2019218_10_20_669_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.0
urn:cite2:ducat:alignments.temp:2019218_10_20_669_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.7
urn:cite2:ducat:alignments.temp:2019218_10_20_669_1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1
urn:cite2:ducat:alignments.temp:2019218_10_20_669_1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1
urn:cite2:ducat:alignments.temp:2019218_10_20_669_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0
urn:cite2:ducat:alignments.temp:2019218_10_20_669_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.5
urn:cite2:ducat:alignments.temp:2019218_10_20_669_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.6
urn:cite2:ducat:alignments.temp:2019218_10_20_669_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.4
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9
urn:cite2:ducat:alignments.temp:2019218_10_20_669_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10
urn:cite2:ducat:alignments.temp:2019218_10_20_669_4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6
urn:cite2:ducat:alignments.temp:2019218_10_20_669_4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.2
urn:cite2:ducat:alignments.temp:2019218_10_20_669_4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12
urn:cite2:ducat:alignments.temp:2019218_10_20_669_5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8
urn:cite2:ducat:alignments.temp:2019218_10_20_669_5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.13

"""

}
