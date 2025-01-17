package edu.furman.classics.citealign
import org.scalatest.FlatSpec
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._


/**
*/
class CiteAlignmentSpec extends FlatSpec {

  "A CiteAlignment" should "build" in {

  	val uVec:Vector[CtsUrn] = Vector(
  		CtsUrn("urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.1-1.2"),
  		CtsUrn("urn:cts:fufolio:pope.iliad.fu2019:1.1.1-1.1.2"),
  		CtsUrn("urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.3-1.4"),
  		CtsUrn("urn:cts:fufolio:pope.iliad.fu2019:1.1.3-1.1.4"),
  		CtsUrn("urn:cts:greekLit:tlg0012.tlg001.perseus_grc2:1.4-1.5"),
  		CtsUrn("urn:cts:fufolio:pope.iliad.fu2019:1.1.5-1.1.6")
  	)
  	val aUrn:Cite2Urn = Cite2Urn("urn:cite2:fufolio:iliadAlign.blackwell1:1")
  	val label:String = "Iliad Test Alignment"
  	val ca:CiteAlignment = CiteAlignment(aUrn, label, uVec)
  	assert(ca.passages.size == 6)
  	
  }

}
