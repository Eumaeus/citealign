package edu.furman.classics.citealign

import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.ohco2._

import scala.scalajs.js
import scala.scalajs.js.annotation._


@JSExportAll  case class CiteAlignment(
  urn: Cite2Urn,
  label: String,
  description: String,
  passages:Vector[CtsUrn]
  ) {



}
