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
  * @constructor create a new [[CiteAlignment]] class 
  */

@JSExportAll  case class CiteAlignment(
  urn: Cite2Urn,
  label: String,
  description: String,
  passages:Vector[CtsUrn]
  ) {



}
