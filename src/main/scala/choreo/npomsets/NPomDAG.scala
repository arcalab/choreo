package choreo.npomsets

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset._
import choreo.datastructures.{DAGIso, DAG}
import choreo.datastructures.DAG._
import choreo.datastructures.Isomorphism.IsoResult

/**
 * Created by guillecledou on 12/07/2021
 */

object NPomDAG:
  def apply(p:NPomset):DAG[Event] = DAG.fromPred(p.events.toSet,p.pred)

  def areIsomorphic(p1:NPomset,p2:NPomset):IsoResult[Event,Event] =
    DAGIso.areIsomorphic(NPomDAG(p1),NPomDAG(p2),
      (e1:Event,e2:Event)=>p1.actions(e1)==p2.actions(e2))

  def prefixes(p:NPomset):Set[NPomset] =
    for d <- NPomDAG(p).prefixDAGs() yield
      (NPomset(p.events,p.actions,d.predecessors,p.loop) --
        (p.events.toSet--d.nodes)).simplifiedFull
