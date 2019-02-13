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

	private val dataModelUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:alignment")
	val relationUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:aligns")
	val textRepo:Option[TextRepository] = library.textRepository
	val collRepo:Option[CiteCollectionRepository] = library.collectionRepository
	val dataModels:Option[Vector[DataModel]] = library.dataModels	
	val relations:Option[CiteRelationSet] = library.relationSet
	val collections:Vector[Cite2Urn] = library.collectionsForModel(Cite2Urn("urn:cite2:cite:datamodels.v1:alignment"))
	val hasCollection:Boolean = collections.size > 0
	val hasAlignmentRelations:Boolean = {
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


	

}
