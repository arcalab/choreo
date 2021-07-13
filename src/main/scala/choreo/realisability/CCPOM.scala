package choreo.realisability

import choreo.Utils
import choreo.datastructures.Isomorphism.{IsoResult, Isomorphisms}
import choreo.syntax.Agent
import choreo.npomsets._
import choreo.npomsets.NPomset._
import choreo.npomsets.NPomIso._
import choreo.datastructures._



/**
 * Created by guillecledou on 12/07/2021
 */

object CCPOM:

  type CC2Res = Set[CC2LocalRes]
  type CC2LocalRes = (NPomset,Option[(NPomset,Isomorphisms[Event,Event])])

  def cc2(p:NPomset): CC2Res =
    val refinments = p.refinements.map(_.simplifiedFull)
    for ic<-IC(p) yield cc2(ic.getPom.simplifiedFull,refinments)

  def cc2(local:NPomset,global:Set[NPomset]): CC2LocalRes =
    //todo can be cut when found (unless we want to keep all isos)
    val res = for g <- global yield (g,areIsomorphic(g,local))
    res.find(r=>r._2.isDefined) match
      case Some((g,Some(iso))) => (local,Some((g,iso)))
      case _ => (local,None)

  def pp(r:CC2Res):String =
    (if r.forall(l=>l._2.isDefined) then "CC2 is satisfied"
    else "CC2 is not satisfied") ++"\n"++
      r.map(l=>pp(l)).mkString("\n")

  def pp(l:CC2LocalRes):String =
    if l._2.isDefined then
      s"""- Local pomset: ${l._1}
         |- Global pomset ${l._2.get._1}
         |are isomorphic by the following isomorphism
         |${Isomorphism.ppIsos(l._2.get._2)}
         |""".stripMargin
    else
      s"""- Local pomset: ${l._1}
         |is not isomorphic to any global pomset
         |""".stripMargin



