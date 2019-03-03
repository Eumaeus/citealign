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
class CiteAlignmentPerformance2Spec extends FlatSpec {

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
    assert (alignments.size == 25 )
  }

  it should "return a vector of URNs for all aligment-objects in a library" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrns:Vector[Cite2Urn] = cam.alignmentUrns
    assert (alignmentUrns.size == 25 )
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
    assert (alignments.size == 25 )
  }

  it should "return a vector of URNs for all aligment-objects in a library filtered by Cite2Urn" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0")
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
    val u:Cite2Urn = Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0")
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
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0"),
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


/*
  it should "list alignments for a text" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentsPresent:Set[Cite2Urn] = Set(
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_2"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_3"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_4"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_5")
    )
    val alignments:Set[CiteAlignment] = cam.alignmentsForText(textUrn)
    val aligmentUrns:Set[Cite2Urn] = alignments.map(_.urn)
    assert( aligmentUrns == alignmentsPresent )
  }
  */
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

/*
it should "list alignments for a passage" in {
    val passage:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)

    val expectedSet = cam.getAlignments(Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0")).toSet
    val als:Set[CiteAlignment] = cam.alignmentsForText(passage)
    assert( als == expectedSet)

  }
  */

/*
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
  */

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

  it should "load aligments in a reasonably short time" in {
var time0 = new js.Date().getTime()
    val lib:CiteLibrary = loadLibrary()
    val am:CiteAlignmentManager = CiteAlignmentManager(lib)
    val u:CtsUrn = CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22") 
    val vCorp:edu.holycross.shot.ohco2.Corpus = lib.textRepository.get.corpus >= u
    val als:Vector[Cite2Urn] = am.alignmentsForText(u).map(_.urn).toVector
    if (als.size > 0) { 
      val c:edu.holycross.shot.ohco2.Corpus = am.corpusForAlignments(als, false)
    }
var timeEnd = new js.Date().getTime()
println(s"Ran in ${(timeEnd - time0) / 1000 } seconds")
  }
  

  it should "return a Corpus for a vector of alignments" in {
//var time0 = new js.Date().getTime()
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignVec:Vector[Cite2Urn] = Vector(Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_2"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_3"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_4"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_5"))
    val testCorp:edu.holycross.shot.ohco2.Corpus = cam.corpusForAlignments(alignVec)
//var timeEnd = new js.Date().getTime()
//println(s"Ran (false) in ${(timeEnd - time0) / 1000 } seconds")
    assert(testCorp.size == 22)
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc:")).nodes(0).urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0"))
    assert((testCorp ~~ CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng:")).last.urn == CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.13"))
  }

  it should "return a Corpus for a vector of alignments, expanded to the containing element" in {
//var time0 = new js.Date().getTime()
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignVec:Vector[Cite2Urn] = Vector(Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_0"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019218_10_20_669_1"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_2"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_3"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_4"),
      Cite2Urn("urn:cite2:ducat:alignments.temp:2019210_57_31_172_5"))
    val testCorp:edu.holycross.shot.ohco2.Corpus = cam.corpusForAlignments(alignVec, true)
//var timeEnd = new js.Date().getTime()
//println(s"Ran (true) in ${(timeEnd - time0) / 1000 } seconds")
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
name#CITE Library generated by the Ducat application, Sat Mar 02 2019 10:52:28 GMT+0100 (CET)
urn#urn:cite2:cex:ducatauto.20192:10_52_28_542
license#CC Share Alike.
        

// URL for state at export:
// file:///Users/cblackwell/Dropbox/CITE/scala/reader-app/target/scala-2.12/classes/index-dev.html?urn=urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22&urn=urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22&urn=


#!ctscatalog
urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:#book/section/token#Herodotus#Histories#Greek, Godley, ed.#tokenized#true#grc
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:#book/section/token#Herodotus#Histories#English, trans. Godley#tokenized, no punctuation#true#eng

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
urn:cite2:ducat:alignments.temp:2019210_57_31_172_0#Alignment 0#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_1#Alignment 1#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#Alignment 2#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_3#Alignment 3#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_4#Alignment 4#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_5#Alignment 5#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#Alignment 6#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_7#Alignment 7#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#Alignment 8#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_172_9#Alignment 9#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_10#Alignment 10#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_11#Alignment 11#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_12#Alignment 12#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#Alignment 13#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#Alignment 14#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_15#Alignment 15#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_16#Alignment 16#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_17#Alignment 17#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_18#Alignment 18#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#Alignment 19#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#Alignment 20#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_21#Alignment 21#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_22#Alignment 22#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_23#Alignment 23#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT
urn:cite2:ducat:alignments.temp:2019210_57_31_173_24#Alignment 24#Textual Alignment created with the Ducat tool on: Sat Mar 02 2019 10:57:31 GMT+0100 (CET).#CWB#Sat, 02 Mar 2019 09:57:31 GMT

#!relations
urn:cite2:ducat:alignments.temp:2019210_57_31_172_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0
urn:cite2:ducat:alignments.temp:2019210_57_31_172_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.5
urn:cite2:ducat:alignments.temp:2019210_57_31_172_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.6
urn:cite2:ducat:alignments.temp:2019210_57_31_172_0#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.7
urn:cite2:ducat:alignments.temp:2019210_57_31_172_1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1
urn:cite2:ducat:alignments.temp:2019210_57_31_172_1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.4
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.8
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.9
urn:cite2:ducat:alignments.temp:2019210_57_31_172_2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.10
urn:cite2:ducat:alignments.temp:2019210_57_31_172_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6
urn:cite2:ducat:alignments.temp:2019210_57_31_172_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.2
urn:cite2:ducat:alignments.temp:2019210_57_31_172_3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12
urn:cite2:ducat:alignments.temp:2019210_57_31_172_4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.7
urn:cite2:ducat:alignments.temp:2019210_57_31_172_4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.0
urn:cite2:ducat:alignments.temp:2019210_57_31_172_5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8
urn:cite2:ducat:alignments.temp:2019210_57_31_172_5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.13
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.9
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.11
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.12
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.14
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.15
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.16
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.17
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.18
urn:cite2:ducat:alignments.temp:2019210_57_31_172_6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.19
urn:cite2:ducat:alignments.temp:2019210_57_31_172_7#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.14
urn:cite2:ducat:alignments.temp:2019210_57_31_172_7#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.21
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.15
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.16
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.17
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.23
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.24
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.25
urn:cite2:ducat:alignments.temp:2019210_57_31_172_8#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.26
urn:cite2:ducat:alignments.temp:2019210_57_31_172_9#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.18
urn:cite2:ducat:alignments.temp:2019210_57_31_172_9#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.22
urn:cite2:ducat:alignments.temp:2019210_57_31_173_10#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.20
urn:cite2:ducat:alignments.temp:2019210_57_31_173_10#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.28
urn:cite2:ducat:alignments.temp:2019210_57_31_173_11#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.21
urn:cite2:ducat:alignments.temp:2019210_57_31_173_11#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.29
urn:cite2:ducat:alignments.temp:2019210_57_31_173_11#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.30
urn:cite2:ducat:alignments.temp:2019210_57_31_173_12#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.22
urn:cite2:ducat:alignments.temp:2019210_57_31_173_12#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.32
urn:cite2:ducat:alignments.temp:2019210_57_31_173_12#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.33
urn:cite2:ducat:alignments.temp:2019210_57_31_173_12#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.34
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.23
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.24
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.25
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.37
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.38
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.39
urn:cite2:ducat:alignments.temp:2019210_57_31_173_13#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.40
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.26
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.27
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.28
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.35
urn:cite2:ducat:alignments.temp:2019210_57_31_173_14#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.36
urn:cite2:ducat:alignments.temp:2019210_57_31_173_15#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.29
urn:cite2:ducat:alignments.temp:2019210_57_31_173_15#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.31
urn:cite2:ducat:alignments.temp:2019210_57_31_173_16#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.31
urn:cite2:ducat:alignments.temp:2019210_57_31_173_16#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.33
urn:cite2:ducat:alignments.temp:2019210_57_31_173_16#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.42
urn:cite2:ducat:alignments.temp:2019210_57_31_173_16#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.43
urn:cite2:ducat:alignments.temp:2019210_57_31_173_17#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.34
urn:cite2:ducat:alignments.temp:2019210_57_31_173_17#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.35
urn:cite2:ducat:alignments.temp:2019210_57_31_173_17#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.44
urn:cite2:ducat:alignments.temp:2019210_57_31_173_17#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.45
urn:cite2:ducat:alignments.temp:2019210_57_31_173_18#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.38
urn:cite2:ducat:alignments.temp:2019210_57_31_173_18#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.39
urn:cite2:ducat:alignments.temp:2019210_57_31_173_18#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.46
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.41
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.42
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.43
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.48
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.49
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.50
urn:cite2:ducat:alignments.temp:2019210_57_31_173_19#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.51
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.44
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.45
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.46
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.55
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.56
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.57
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.58
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.59
urn:cite2:ducat:alignments.temp:2019210_57_31_173_20#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.60
urn:cite2:ducat:alignments.temp:2019210_57_31_173_21#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.47
urn:cite2:ducat:alignments.temp:2019210_57_31_173_21#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.52
urn:cite2:ducat:alignments.temp:2019210_57_31_173_21#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.53
urn:cite2:ducat:alignments.temp:2019210_57_31_173_21#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.54
urn:cite2:ducat:alignments.temp:2019210_57_31_173_22#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.48
urn:cite2:ducat:alignments.temp:2019210_57_31_173_22#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.61
urn:cite2:ducat:alignments.temp:2019210_57_31_173_23#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.49
urn:cite2:ducat:alignments.temp:2019210_57_31_173_23#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.50
urn:cite2:ducat:alignments.temp:2019210_57_31_173_23#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.65
urn:cite2:ducat:alignments.temp:2019210_57_31_173_24#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.51
urn:cite2:ducat:alignments.temp:2019210_57_31_173_24#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.62
urn:cite2:ducat:alignments.temp:2019210_57_31_173_24#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.63
urn:cite2:ducat:alignments.temp:2019210_57_31_173_24#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.64

"""

}
