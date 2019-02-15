package edu.furman.classics.citealign
import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import scala.io.Source


/**
*/
class CiteAlignmentManagerSpec extends FlatSpec {

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

  it should "have a working editLinesInCex function in the tests" in {
    val goodStr = "aaa\nbbb\nccc"
    val badStr = editLinesInCex(goodStr,2,"XXX")
    assert( badStr == "aaa\nXXX\nccc")
  }

  it should "fail to buld gracefully if the datamodel is not defined in the library" in {
    val badCex:String = removeLinesFromCex(goodCex,Vector(11, 12))
    val lib:CiteLibrary = loadLibrary(badCex)
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    assert(cam.isValid == false)
  }

  it should "fail to build gracefully if there is no implementing collection" in pending
  it should "fail to build gracefully if there is no relationSet" in pending
  it should "fail to build gracefully if there is no textRepository" in pending
  it should "fail to build gracefully if there is are no alignmentRelations" in pending
  it should "throw an exception if any alignment-object URN is not present in a collection" in pending
  it should "throw an exception if any aligned text URN is not present in a collection" in pending

  it should "throw an exception if the subject of all relations is not a Cite2Urn" in {
    val badLine:String = "urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4-1.5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4-1.5"
    val badCex:String = editLinesInCex(goodCex,63,badLine)
    val lib:CiteLibrary = loadLibrary(badCex)
    val thrown = intercept[Exception] {
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    }
    assert( thrown.getMessage.size > 0)
  }

  it should "throw an exception if the object of all relations is not a CtsUrn" in {
    val badLine:String = "urn:cite2:fufolio:iliadAlign.blackwell:5#urn:cite2:cite:verbs.v1:aligns#urn:cite2:fufolio:iliadAlign.blackwell:5"
    val badCex:String = editLinesInCex(goodCex,63,badLine)
    val lib:CiteLibrary = loadLibrary(badCex)
    val thrown = intercept[Exception] {
      val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    }
    assert( thrown.getMessage.size > 0)
  }

  it should "return a vector of urns to collections that record alignments" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val colls:Vector[Cite2Urn] = cam.alignmentCollections
    assert( colls.size == 2 )
    assert( colls.contains( Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell:")))
    assert( colls.contains( Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:")))
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
    val u:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignments:Vector[CiteObject] = cam.alignments(u)
    assert (alignments.size == 2 )
  }

  it should "return a vector of URNs for all aligment-objects in a library filtered by Cite2Urn" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrns:Vector[Cite2Urn] = cam.alignmentUrns(u)
    assert (alignmentUrns.size == 2 )
    alignmentUrns(0).asInstanceOf[Urn] match {
      case CtsUrn(_) => assert (false)
      case Cite2Urn(_) => assert (true)
      case _ => assert(false)
    }
  }

  it should "return a Vector of CtsUrns of texts participating in an alignment defined by an object-level URN" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"))
    assert( cam.textsAligned(u).toSet == textsPresent.toSet)
  }

  it should "return a Vector of CtsUrns of texts participating in an alignment defined by a collection-level URN" in {
    val u:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:")
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val textsPresent:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"))
    assert( cam.textsAligned(u).toSet == textsPresent.toSet )
  }

  it should "list texts participating in a vector of alignments" in {
    val u:Vector[Cite2Urn] = Vector(
      Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1"),
      Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:2"),
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
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:"),
      CtsUrn("urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:"),
      CtsUrn("urn:cts:fufolio:pope.iliad.fu2019:")
    )
    assert( cam.textsAligned.toSet == textsPresent.toSet )
  }

  it should "construct a CiteAlignment object from a Cite2Urn" in {
    val lib:CiteLibrary = loadLibrary()
    val cam:CiteAlignmentManager = CiteAlignmentManager(lib)
    val alignmentUrn:Cite2Urn = Cite2Urn("urn:cite2:fufolio:hdtAlign.blackwell:1")
    val passages:Vector[CtsUrn] = Vector(
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0-8.22.6"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1"),
      CtsUrn("urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3-8.22.12")
    )
    val alignment:Vector[CiteAlignment] = cam.getAlignment(alignmentUrn)
    assert( alignment.size == 1)
    assert( alignment.head.urn == alignmentUrn)
    assert( alignment.head.passages == passages)
  }

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
      assert(expected == compressed)

  }

  it should "list alignments for a text" in {
    val textUrn:CtsUrn = CtsUrn("urn:cts:fufolio:pope.iliad.fu2019:")
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
    assert( cam.alignmentsForText(textUrn).toSet == alignmentsPresent.toSet )
    val alignments:Set[CiteAlignment] = cam.alignmentsForText(textUrn)
    val aligmentUrns:Set[Cite2Urn] = alignments.map(_.urn)
    assert( aligmentUrns == alignmentsPresent )
  }

  it should "list alignments for a passage" in pending

  it should "list alignments for a range" in pending

  it should "list alignments for a vector of CtsUrns" in pending

  it should "return a Vector[CtsUrn] for an alignment" in pending

  it should "return a Corpus for a vector of alignments" in pending

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

val goodCex:String = """#!cexversion
3.0

#!citelibrary
name#Fragment from Herodotus' Histories, Book VIII on Papyrus Oxyrhynchus 2099, dated to early 2nd century AD.
urn#urn:cite2:cex:fufolio.2018a:POxy2099
license#CC Share Alike.  For details, see more info.

#!datamodels
Collection#Model#Label#Description
urn:cite2:fufolio:hdtAlign.blackwell:#urn:cite2:cite:datamodels.v1:alignment#Text Alignment Model#The CITE model for text alignment. See documentation at <https://eumaeus.github.io/citealign/>.
urn:cite2:fufolio:iliadAlign.blackwell:#urn:cite2:cite:datamodels.v1:alignment#Text Alignment Model#The CITE model for text alignment. See documentation at <https://eumaeus.github.io/citealign/>.

#!citecollections
URN#Description#Labelling property#Ordering property#License
urn:cite2:cite:datamodels.v1:#CITE data models#urn:cite2:cite:datamodels.v1.label:##Public domain
urn:cite2:cite:verbs.v1:#Collection of verbal relations#urn:cite2:cite:verbs.v1.label:##Public Domain

urn:cite2:fufolio:hdtAlign.blackwell:#Translation alignments#urn:cite2:fufolio:hdtAlign.blackwell.label:##Public Domain

urn:cite2:fufolio:iliadAlign.blackwell:#Translation alignments#urn:cite2:fufolio:iliadAlign.blackwell.label:##Public Domain

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:fufolio:hdtAlign.blackwell.urn:#Alignment Record#Cite2Urn#
urn:cite2:fufolio:hdtAlign.blackwell.label:#Label#String#
urn:cite2:fufolio:hdtAlign.blackwell.description:#Description#String#
urn:cite2:fufolio:hdtAlign.blackwell.editor:#Editor#String#
urn:cite2:fufolio:hdtAlign.blackwell.date:#Date#String#

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:fufolio:iliadAlign.blackwell.urn:#Alignment Record#Cite2Urn#
urn:cite2:fufolio:iliadAlign.blackwell.label:#Label#String#
urn:cite2:fufolio:iliadAlign.blackwell.description:#Description#String#
urn:cite2:fufolio:iliadAlign.blackwell.editor:#Editor#String#
urn:cite2:fufolio:iliadAlign.blackwell.date:#Date#String#

#!citedata
urn#label#description#editor#date
urn:cite2:fufolio:hdtAlign.blackwell:1#Hdt. 1#Herodotus Alignment 1#cwb#2/12/2019
urn:cite2:fufolio:hdtAlign.blackwell:2#Hdt. 2#Herodotus Alignment 2#cwb#2/12/2019

#!citedata
urn#label#description#editor#date
urn:cite2:fufolio:iliadAlign.blackwell:3#Iliad 1#Iliad Alignment 1#cwb#2/12/2019
urn:cite2:fufolio:iliadAlign.blackwell:4#Iliad 2#Iliad Alignment 2#cwb#2/12/2019
urn:cite2:fufolio:iliadAlign.blackwell:5#Iliad 3#Iliad Alignment 3#cwb#2/12/2019
urn:cite2:fufolio:iliadAlign.blackwell:6#Iliad 4#Iliad Alignment 4#cwb#2/12/2019

#!relations
// Hdt.
urn:cite2:fufolio:hdtAlign.blackwell:1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0-8.22.6
urn:cite2:fufolio:hdtAlign.blackwell:1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.3-8.22.12
urn:cite2:fufolio:hdtAlign.blackwell:1#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.1
urn:cite2:fufolio:hdtAlign.blackwell:2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6-8.22.7
urn:cite2:fufolio:hdtAlign.blackwell:2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.0
urn:cite2:fufolio:hdtAlign.blackwell:2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.2
urn:cite2:fufolio:hdtAlign.blackwell:2#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0016.tlg001.eng.tokens:8.22.12
// Iliad greek - pope
urn:cite2:fufolio:iliadAlign.blackwell:3#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.1-1.2
urn:cite2:fufolio:iliadAlign.blackwell:3#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.1.1-1.1.2
urn:cite2:fufolio:iliadAlign.blackwell:4#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.3-1.4
urn:cite2:fufolio:iliadAlign.blackwell:4#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.1.3-1.1.4
urn:cite2:fufolio:iliadAlign.blackwell:5#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4-1.5
urn:cite2:fufolio:iliadAlign.blackwell:5#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.1.5-1.1.6
urn:cite2:fufolio:iliadAlign.blackwell:5#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.1.8
urn:cite2:fufolio:iliadAlign.blackwell:6#urn:cite2:cite:verbs.v1:aligns#urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.6-1.10
urn:cite2:fufolio:iliadAlign.blackwell:6#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.1.7
urn:cite2:fufolio:iliadAlign.blackwell:6#urn:cite2:cite:verbs.v1:aligns#urn:cts:fufolio:pope.iliad.fu2019:1.2.1-1.2.4

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:cite:verbs.v1.urn:#URN#Cite2Urn#
urn:cite2:cite:verbs.v1.label:#label#String#
urn:cite2:cite:verbs.v1.description:#description#String#

#!citedata
urn#label#description
urn:cite2:cite:verbs.v1:commentsOn#comments on#subject[Urn] comments on object[Urn]
urn:cite2:cite:verbs.v1:illustrates#illustrates#subject[Urn] comments on object[Urn]
urn:cite2:cite:verbs.v1:hasOnIt#has on it#subject[Urn] comments on object[Urn]
urn:cite2:cite:verbs.v1:aligns#aligns#subject[CiteUrn] is an alignment that includes passage[CtsUrn]

#!citeproperties
Property#Label#Type#Authority list
urn:cite2:cite:datamodels.v1.urn:#Data model#Cite2Urn#
urn:cite2:cite:datamodels.v1.label:#Label#String#
urn:cite2:cite:datamodels.v1.description:#Description#String#


#!ctscatalog
urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:#book/section/token#Herodotus#Histories#Greek, Godley, ed.#tokenized#true#grc
urn:cts:greekLit:tlg0016.tlg001.eng.tokens:#book/section/token#Herodotus#Histories#English, trans. Godley#tokenized, no punctuation#true#eng
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:#book/line#Homeric Epic#Iliad#Greek. Allen, ed. Perseus Digital Library. Creative Commons Attribution 3.0 License##true#grc
urn:cts:fufolio:pope.iliad.fu2019:#book, stanza, line#Alexaner Pope#Iliad#Furman Ed. 2019##true#eng

#!ctsdata
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.0#Ἀθηναίων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.1#δὲ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.2#νέας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.3#τὰς
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.4#ἄριστα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.5#πλεούσας
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.6#ἐπιλεξάμενος
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.7#Θεμιστοκλέης
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.8#ἐπορεύετο
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.9#περὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.10#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.11#πότιμα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.12#ὕδατα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.14#ἐντάμνων
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.15#ἐν
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.16#τοῖσι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.17#λίθοισι
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.18#γράμματα
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.20#τὰ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.21#Ἴωνες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.22#ἐπελθόντες
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.23#τῇ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.24#ὑστεραίῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.25#ἡμέρῃ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.26#ἐπὶ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.27#τὸ
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.28#Ἀρτεμίσιον
urn:cts:greekLit:tlg0016.tlg001.grc.tokens:8.22.29#ἐπελέξαντο

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

#!ctsdata
// Allen Iliad
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.1#Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.2#οὐλομένην, ἣ μυρί᾽ Ἀχαιοῖς ἄλγε᾽ ἔθηκε,
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.3#πολλὰς δ᾽ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4#ἡρώων, αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.5#οἰωνοῖσί τε πᾶσι, Διὸς δ᾽ ἐτελείετο βουλή,
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.6#ἐξ οὗ δὴ τὰ πρῶτα διαστήτην ἐρίσαντε
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.7#Ἀτρεΐδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς.
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.8#Τίς τάρ σφωε θεῶν ἔριδι ξυνέηκε μάχεσθαι;
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.9#Λητοῦς καὶ Διὸς υἱός· ὃ γὰρ βασιλῆϊ χολωθεὶς
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.10#νοῦσον ἀνὰ στρατὸν ὄρσε κακήν, ὀλέκοντο δὲ λαοί,
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.11#οὕνεκα τὸν Χρύσην ἠτίμασεν ἀρητῆρα
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.12#Ἀτρεΐδης· ὃ γὰρ ἦλθε θοὰς ἐπὶ νῆας Ἀχαιῶν
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.13#λυσόμενός τε θύγατρα φέρων τ᾽ ἀπερείσι᾽ ἄποινα,
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.14#στέμματ᾽ ἔχων ἐν χερσὶν ἑκηβόλου Ἀπόλλωνος
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.15#χρυσέῳ ἀνὰ σκήπτρῳ, καὶ λίσσετο πάντας Ἀχαιούς,
urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.16#Ἀτρεΐδα δὲ μάλιστα δύω, κοσμήτορε λαῶν·

#!ctsdata
urn:cts:fufolio:pope.iliad.fu2019:1.1.1#Achilles' wrath, to Greece the direful spring
urn:cts:fufolio:pope.iliad.fu2019:1.1.2#Of woes unnumber'd, heavenly goddess, sing!
urn:cts:fufolio:pope.iliad.fu2019:1.1.3#That wrath which hurl'd to Pluto's gloomy reign
urn:cts:fufolio:pope.iliad.fu2019:1.1.4#The souls of mighty chiefs untimely slain;
urn:cts:fufolio:pope.iliad.fu2019:1.1.5#Whose limbs unburied on the naked shore,
urn:cts:fufolio:pope.iliad.fu2019:1.1.6#Devouring dogs and hungry vultures tore.
urn:cts:fufolio:pope.iliad.fu2019:1.1.7#Since great Achilles and Atrides strove,
urn:cts:fufolio:pope.iliad.fu2019:1.1.8#Such was the sovereign doom, and such the will of Jove!
urn:cts:fufolio:pope.iliad.fu2019:1.2.1#Declare, O Muse! in what ill-fated hour
urn:cts:fufolio:pope.iliad.fu2019:1.2.2#Sprung the fierce strife, from what offended power
urn:cts:fufolio:pope.iliad.fu2019:1.2.3#Latona's son a dire contagion spread,
urn:cts:fufolio:pope.iliad.fu2019:1.2.4#And heap'd the camp with mountains of the dead;
urn:cts:fufolio:pope.iliad.fu2019:1.2.5#The king of men his reverent priest defied,
urn:cts:fufolio:pope.iliad.fu2019:1.2.6#And for the king's offence the people died.
urn:cts:fufolio:pope.iliad.fu2019:1.3.1#For Chryses sought with costly gifts to gain
urn:cts:fufolio:pope.iliad.fu2019:1.3.2#His captive daughter from the victor's chain.
urn:cts:fufolio:pope.iliad.fu2019:1.3.3#Suppliant the venerable father stands;
urn:cts:fufolio:pope.iliad.fu2019:1.3.4#Apollo's awful ensigns grace his hands
urn:cts:fufolio:pope.iliad.fu2019:1.3.5#By these he begs; and lowly bending down,
urn:cts:fufolio:pope.iliad.fu2019:1.3.6#Extends the sceptre and the laurel crown
urn:cts:fufolio:pope.iliad.fu2019:1.3.7#He sued to all, but chief implored for grace
urn:cts:fufolio:pope.iliad.fu2019:1.3.8#The brother-kings, of Atreus' royal race

"""

}
