package choreo.npomsets

import choreo.datastructures.Isomorphism._
import choreo.datastructures.DAGIso
import choreo.npomsets.NPomDAG
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset._


/**
 * Created by guillecledou on 12/07/2021
 */

object NPomIso:

  def areIsomorphic(p1:NPomset,p2:NPomset):IsoResult[Event,Event] =
    DAGIso.areIsomorphic(NPomDAG(p1),NPomDAG(p2),
      (e1:Event,e2:Event)=>p1.actions(e1)==p2.actions(e2))

  //def areIsomorphic(p1: NPomset,
  //                           p2: NPomset,
  //                           comp: ((Event, Event)) => Boolean): IsoResult[Event,Event] =
  //  DAGVFIso.areIsomorphic[Event,Event](NPomDAG(p1),NPomDAG(p2),comp)

