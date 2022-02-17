package choreo.petrinet

import choreo.petrinet.PN.*
import choreo.syntax.Choreo.{In, Out}

/**
 * Created by guillecledou on 14/02/2022
 */
@deprecated
case class PN(places: Set[Place],
              transitions: Set[Trans],
              marking: Set[PlaceId],
              name:String)

object PN:

  type PlaceId = Int
  type TransId = Int
  type NodeId = Int

  def apply():PN = PN(Set(),Set(),Set(),"")

  case class Place(id:Int)

  // todo: channel it to dependent, but if we want to access
  //  the sender, receiver, and msg, is the easiest way for now
  case class Trans(id:TransId, channel:In|Out, pre:Set[PlaceId], post:Set[PlaceId])
