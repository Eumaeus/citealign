package edu.furman.classics.citealign

import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.ohco2._

import scala.scalajs.js
import scala.scalajs.js.annotation._

  /** A Class for creating a CiteAlignment class
  *
  * @constructor create a new [[CiteAlignmentManager]] class 
  */

@JSExportAll  case class CiteAlignmentManager(library: CiteLibrary) {

	/* To be valid, we need:
	*		- a text repository
	*		- at least one collection assocaited with datamodel:
	*			urn:cite2:cite:datamodels.v1:alignment
	*		- data for that/those collections
	*		- objects in those collections represented in relations with:
	*			urn:cite2:cite:verbs.v1:aligns
	*/

	// Private values
	private val dataModelUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:alignment")
	private val relationUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:aligns")
	private val textRepo:Option[TextRepository] = library.textRepository
	private val collRepo:Option[CiteCollectionRepository] = library.collectionRepository
	private val dataModels:Option[Vector[DataModel]] = library.dataModels	
	private val relations:Option[CiteRelationSet] = library.relationSet
	private val collections:Vector[Cite2Urn] = library.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:alignment"))
	private val hasCollection:Boolean = collections.size > 0
	private val hasAlignmentRelations:Boolean = {
		library.relationSet match {
			case Some(rs) => {
				rs.verbs.contains(relationUrn)
			}
			case None => false
		}
	}

	// Check for validity
	var isValid:Boolean = true
	if (textRepo == None) isValid = false
	if (collRepo == None) isValid = false
	if (dataModels == None) isValid = false
	if (relations == None) isValid = false
	if (hasCollection == false) isValid = false
	if (hasAlignmentRelations == false) isValid = false

	// Check that the relations are okay
	if (isValid){
		hasAlignmentRelations match {
			case true => {
				val alignmentRels:CiteRelationSet = relations.get.verb(relationUrn)
				val badSubjects:Set[CiteTriple] = {
					alignmentRels.relations.filter(ct => {
						ct.urn1 match {
							case Cite2Urn(_) => false 
							case _ => true 
						}
					})
				}
				val badObjects:Set[CiteTriple] = {
					alignmentRels.relations.filter(ct => {
						ct.urn2 match {
							case CtsUrn(_) => false 
							case _ => true 
						}
					})
				}
				val badRelations = badSubjects ++ badObjects
				if (badRelations.size > 0) {
					isValid = false
					throw new Exception(s"""Bad alignment relations: ${badRelations.mkString("\n\t")}""")
				}
			}	
			case false => isValid = false
		}
	}

	/** Returns Collections that implement the Alignment Data Model
	*
	**/
	val alignmentCollections:Vector[Cite2Urn] = collections

	/** Returns all aligments as CiteObjects
	* 
	**/
	def alignments():Vector[CiteObject] = {
		collRepo match {
			case Some(cr) => {
				collections.map(c => {
					cr.objectsForCollection(c)
				}).flatten	
			}
			case None => Vector[CiteObject]()
		}
	} 

	/** Returns all aligments as CiteObjects
	* @param urn filter
	**/
	def alignments(u:Cite2Urn):Vector[CiteObject] = {
		collRepo match {
			case Some(cr) => {
				collections.map(c => {
					cr.objectsForCollection(c).filter(_.urn ~~ u)
				}).flatten	
			}
			case None => Vector[CiteObject]()
		}
	} 

	/** Returns Cite2Urns to all aligments
	* 
	**/
	def alignmentUrns():Vector[Cite2Urn] = {
		collRepo match {
			case Some(cr) => {
			 	alignments.map(_.urn)	
			}
			case None => Vector[Cite2Urn]()
		}
	} 

	/** Returns Cite2Urns to all aligments
	* @param urn Optional filter
	**/
	def alignmentUrns(urn:Cite2Urn):Vector[Cite2Urn] = {
		collRepo match {
			case Some(cr) => {
			 	alignments(urn).map(_.urn)	
			}
			case None => Vector[Cite2Urn]()
		}
	}

	/** Returns CtsUrns representing all texts participating in
	*   an alignment
	* @param urn Cite2Urn The Alignment (object- or collection-level )
	**/

	def textsAligned(urn:Cite2Urn):Vector[CtsUrn] = {
		if (isValid == false) {
			Vector[CtsUrn]()
		} else {
			val aus:Vector[Cite2Urn] = alignmentUrns(urn)
			relations match {
				case Some(rs) => {
					urn.objectComponentOption match {	
						case Some(oc) => {
							val textVec:Vector[CtsUrn] = rs.urn1Match(urn).relations.toVector.map(_.urn2.asInstanceOf[CtsUrn].dropPassage).distinct
							textVec
						}
						case None => {
							val textVec:Vector[CtsUrn] = rs.relations.filter(_.urn1 ~~ urn).toVector.map(_.urn2.asInstanceOf[CtsUrn].dropPassage).distinct
							textVec
						}
					}
				}
				case None => Vector[CtsUrn]()
			}
		}
	}

	/** Returns CtsUrns representing all texts participating in
	*   a Vector of alignment
	* @param urns Vector[Cite2Urn] The Alignments (object- or collection-level )
	**/
	def textsAligned(urns:Vector[Cite2Urn]):Vector[CtsUrn] = {
		if (isValid == false) {
			Vector[CtsUrn]()
		} else {
			val aus:Vector[Cite2Urn] = alignmentUrns()
			relations match {
				case Some(rs) => {
					urns.map(u => textsAligned(u)).flatten
				}
				case None => Vector[CtsUrn]()
			}
		}
	}	

	/** Returns CtsUrns representing all texts participating in
	*   alignments recorded in the library
	**/

	def textsAligned():Vector[CtsUrn] = {
		if (isValid == false) {
			Vector[CtsUrn]()
		} else {
			val aus:Vector[Cite2Urn] = alignmentUrns()
			relations match {
				case Some(rs) => {
					rs.relations.map(_.urn2.asInstanceOf[CtsUrn].dropPassage).toVector.distinct
				}
				case None => Vector[CtsUrn]()
			}
		}
	}	

	/** Given a Cite2Urn, return a Vector of CiteAlignment objects
	* @param urn Cite2Urn
	**/

	def getAlignment(urn:Cite2Urn):Vector[CiteAlignment] = {
		if (isValid) {
			relations match {
				case Some(rs) => {
					val alignmentObjects:Vector[CiteObject] = alignments(urn)		
					val alignmentVec:Vector[CiteAlignment] = {
						alignmentObjects.map( ao => {
							val urn:Cite2Urn = ao.urn
							val label:String = ao.label
							val passages:Vector[CtsUrn] = {
								val passageSet:Set[CtsUrn] = rs.urn1Match(urn).relations.map(_.urn2.asInstanceOf[CtsUrn])
								val passageVec:Vector[CtsUrn] = compressReff(sortPassages(passageSet))
								passageVec
							}
							CiteAlignment(urn, label, passages)
						})
					}
					alignmentVec
				}
				case None => Vector[CiteAlignment]()
			}
		} else { 
			Vector[CiteAlignment]()
		}
	}

	/** Given a Set[CtsUrn] return a Vector[CtsUrn] sorted by document
	*   order according to the Corpus in the TextRepository
	*   @param passageSet Set[CtsUrn]
	**/
	def sortPassages(passageSet:Set[CtsUrn]):Vector[CtsUrn] = {
		if (isValid) {
			textRepo match {
				case Some(tr) => {
					val trc = tr.corpus
					val pv:Vector[CtsUrn] = passageSet.toVector.map(u => trc.validReff(u)).flatten
					val pm:Vector[(CtsUrn,Vector[CtsUrn])] = pv.groupBy(_.dropPassage).toVector
					val workVec:Vector[CtsUrn] = pm.map(work => {
						val thisWorkUrns:Vector[(CtsUrn, Int)] = trc.urns.filter(_.dropPassage == work._1).zipWithIndex
						val theseUrns:Vector[(CtsUrn, Int)] = work._2.map( wu => {
							var thisIndex:Int = thisWorkUrns.find(_._1 == wu).get._2
							(wu, thisIndex)
						})
						theseUrns.sortBy(_._2).map(_._1)
					}).flatten
					workVec
				}
				case None => {
					Vector[CtsUrn]()
				}
			}	
		} else {
			Vector[CtsUrn]()
		}
	}

	/** Given a Vector[CtsUrn] compress it so that any sequences of URNs that
	*   can be expressed as ranges are expressed as ranges.
	* @param urns Vector[CtsUrn]
	**/
	def compressReff(urns:Vector[CtsUrn]):Vector[CtsUrn] = {
		if (isValid) {
			textRepo match {
				case Some(tr) => {
					val trc = tr.corpus
					// expand any ranges
					val pv:Vector[CtsUrn] = urns.toVector.map(u => trc.validReff(u)).flatten
					// group by text
					val pm:Vector[(CtsUrn,Vector[CtsUrn])] = pv.groupBy(_.dropPassage).toVector
					val workVec:Vector[Vector[(CtsUrn,Int)]] = pm.map(work => {
						val thisWorkUrns:Vector[(CtsUrn, Int)] = trc.urns.filter(_.dropPassage == work._1).zipWithIndex
						val theseUrns:Vector[(CtsUrn, Int)] = work._2.map( wu => {
							var thisIndex:Int = thisWorkUrns.find(_._1 == wu).get._2
							(wu, thisIndex)
						})
						//println(s""" \n\ncompressReff sortyBy\n-------${theseUrns.sortBy(_._2).mkString("\n")}\n-------""")
						theseUrns.sortBy(_._2)
					})

					val returnVec:Vector[CtsUrn] = workVec.map(wv => {
						val indices:Vector[Int] = wv.map(_._2)
						val indicesGrouped:Vector[Vector[Int]] = groupSequences(indices)
						val returnDeepVec:Vector[CtsUrn] = indicesGrouped.map( i => {
							if (i.size == 1 ) {
								val u:CtsUrn = wv.find(_._2 == i.head ).get._1
								u
							} else {
								val startUrn:CtsUrn = wv.find(_._2 == i.head ).get._1
								val endUrn:CtsUrn = wv.find(_._2 == i.last ).get._1
								val endPsg:String = endUrn.passageComponent
								val u:CtsUrn = CtsUrn(s"${startUrn}-${endPsg}")
								u
							}						
						})
						returnDeepVec
					}).flatten

					returnVec
				}	
				case None => {
					Vector[CtsUrn]()
				}
			}	
		} else {
			Vector[CtsUrn]()
		}
	}



	/** Given a Vector[CtsUrn] return a Vector[CtsUrn] sorted by document
	*   order according to the Corpus in the TextRepository
	*   @param passageVec Vector[CtsUrn]
	**/
	def sortPassages(passageVec:Vector[CtsUrn]):Vector[CtsUrn] = {
		val passageSet:Set[CtsUrn] = passageVec.toSet
		sortPassages(passageSet)
	}

	/** Returns Cite2Urns representing alignments for given texts, ranges, 
	*   or passages
	*   @param urns Vector[CtsUrn]
	**/
	def alignmentsForText(urns:Vector[CtsUrn]):Set[CiteAlignment] = {
		Set[CiteAlignment]()
	}

	/** Returns Cite2Urns representing alignments for a single text or passage
	*   @param urn CtsUrn
	**/
	def alignmentsForText(urn:CtsUrn):Set[CiteAlignment] = {
		Set[CiteAlignment]()
	}

	/** Utility function: Given a Vector[Int] group all continuous runs
	* @param indices Vector[Int]
	**/
	def groupSequences(indices:Vector[Int]):Vector[Vector[Int]] = {
		val iAsLists:List[Int] = indices.toList
		val answer:List[List[Int]] = groupSequences(iAsLists)
		answer.map(_.toVector).toVector
	}

	/** Utility function: Given a List[Int] group all continuous runs
	* @param indices List[Int]
	**/
	def groupSequences(indices:List[Int]):List[List[Int]] = {
		    val (acc, last) = indices
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
		    answerAsLists
	}


}
