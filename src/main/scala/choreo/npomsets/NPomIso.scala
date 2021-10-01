package choreo.npomsets

import choreo.datastructures.Isomorphism._
import choreo.datastructures.DAGIso
import choreo.npomsets.NPomDAG
import choreo.npomsets.NPomDAG.areIsomorphic
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset._


/**
 * Created by guillecledou on 12/07/2021
 */

object NPomIso:

  def areIsomorphic(p1:NPomset,p2:NPomset):IsoResult[Events,Events] =
    areIsomorphic(p1.events,p1,p2.events,p2)

  def areIsomorphic(n1:Events,p1:NPomset,
                    n2:Events,p2:NPomset): IsoResult[Events,Events] =
    //println(s"[iso Nesting] checnking if ${n1} and $n2 are iso")
    // induced pomset by n1 is isomorphic to induced pomset by n2, and
    if n1.choices.size == n2.choices.size && n1.acts.size == n2.acts.size then
      if NPomDAG.areIsomorphic(inducedNPom(n1,p1),inducedNPom(n2,p2)).isDefined then
        val isoChoices =  areIsomorphic(n1.choices,p1,n2.choices,p2)
        if isoChoices.isDefined then Some(Set(Set((n1,n2))++isoChoices.get.flatten))
        else None
      else None
    else None

  def areIsomorphic(cs1:Set[NChoice[Event]],p1:NPomset,
                    cs2:Set[NChoice[Event]],p2:NPomset):IsoResult[Events,Events] =
    if cs1.isEmpty && cs2.isEmpty then
      Some(Set(Set()))
    else
      val res = for c1<-cs1 yield areIsomorphic(c1,p1,cs2,p2)
      if res.exists(!_.isDefined) then None
      else Some(res.flatMap(r=>r.get))

  def areIsomorphic(c1:NChoice[Event],p1:NPomset,
                    cs2:Set[NChoice[Event]],p2:NPomset):IsoResult[Events,Events] =
    cs2.view.map(c2=>areIsomorphic(c1,p1,c2,p2)).find(_.isDefined) match
      case Some(iso) => iso
      case _ => None

  def areIsomorphic(c1:NChoice[Event],p1:NPomset,
                    c2:NChoice[Event],p2:NPomset):IsoResult[Events,Events] =
    val c1s = Set(c1.left,c1.right)
    val c2s = Set(c2.left,c2.right)
    val res = for n1<-c1s yield
      for n2<-c2s ; n1p = (c1s-n1).head ; n2p = (c2s-n2).head
          iso1 <- areIsomorphic(n1,p1,n2,p2)
          iso2 <- areIsomorphic(n1p,p1,n2p,p2)
      yield Set((n1,n2),(n1p,n2p))
    if res.isEmpty then None else res.headOption
    //
    //lazy val c1lc2l = areIsomorphic(c1.left,p1,c2.left,p2)
    //lazy val c1lc2r = areIsomorphic(c1.left,p1,c2.right,p2)
    //lazy val c1rc2l = areIsomorphic(c1.right,p1,c2.left,p2)
    //lazy val c1rc2r = areIsomorphic(c1.right,p1,c2.right,p2)
    //if c1lc2l.isDefined && c1rc2r.isDefined then
    //  Some(Set(c1.left,c2.left),Set(c1.right,c2.right))
    //else if c1lc2r.isDefined && c1rc2l.isDefined then
    //  Some(Set(c1.left,c2.right),Set(c1.right,c2.left))
    //else if c1rc2l.isDefined && c1lc2r.isDefined =>

  def inducedNPom(n:Events,p:NPomset):NPomset =
    NPomset(Nesting(n.acts,Set(),n.loops),p.actions,p.pred,p.loop).simplifiedFull


//def areIsomorphic(p1:NPomset,p2:NPomset):IsoResult[Event,Event] =
    //DAGIso.areIsomorphic(NPomDAG(p1),NPomDAG(p2),
    //  (e1:Event,e2:Event)=>p1.actions(e1)==p2.actions(e2))

  //def areIsomorphic(p1: NPomset,
  //                           p2: NPomset,
  //                           comp: ((Event, Event)) => Boolean): IsoResult[Event,Event] =
  //  DAGVFIso.areIsomorphic[Event,Event](NPomDAG(p1),NPomDAG(p2),comp)

