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
	

}
