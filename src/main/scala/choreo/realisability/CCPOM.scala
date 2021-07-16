package choreo.realisability

import choreo.Utils
import choreo.datastructures.Isomorphism.{IsoResult, Isomorphisms}
import choreo.syntax.Agent
import choreo.npomsets._
import choreo.npomsets.NPomset._
import choreo.npomsets.NPomDAG._
import choreo.datastructures._
import choreo.realisability.IC

/**
 * Created by guillecledou on 12/07/2021
 */

object CCPOM:

  type CCPomRes = Set[CCPomLocalRes]
  type CCPomLocalRes = (NPomset,Option[(NPomset,Isomorphisms[Event,Event])])

  def cc2(p:NPomset): CCPomRes =
    println(s"[CC2]")
    val refinments = p.refinements.map(_.simplifiedFull)
    (for ic<-IC(p) yield ccpomExists(ic.getPom.simplifiedFull,refinments)).toSet

  def ccpomExists(local:NPomset, global:List[NPomset]): CCPomLocalRes =
    //todo can be cut when found (unless we want to keep all isos)
    val res = for g <- global yield (g,areIsomorphic(g,local))
    res.find(r=>r._2.isDefined) match
      case Some((g,Some(iso))) => (local,Some((g,iso)))
      case _ => (local,None)

  def cc3(p:NPomset):CCPomRes =
    println(s"[CC3]")
    val globalPomsets   = p.refinements.map(_.simplifiedFull)
    val globalPrefixes  = (for g<-globalPomsets yield prefixes(g)).flatten
    val localBranches   = getAllLocalBranches(globalPomsets,p.agents.toSet)
    val localPrefixes   = getAllLocalPrefixes(localBranches)
    val tuples          = getTuples(localPrefixes)
    val ics             = (for t<-tuples; ics<-IC(t.toMap) yield ics).flatten
    println(s"[Global Prefixes] #:${globalPrefixes.size}")
    println(s"[Local Prefixes Per Action] #:\n${localPrefixes.map(p=>s"${p._1}:${p._2.size}").mkString("\n")}")
    println(s"[Local Tuples] #:${tuples.size}")
    println(s"[Interclosures] #:${ics.size}")
    //println(s"[Interclosures] :${ics.map(_.getPom).mkString("\n")}")
    (for (ic<-ics) yield ccpomExists(ic.getPom.simplifiedFull,globalPrefixes)).toSet

  def getAllLocalBranches(globals:List[NPomset],agents:Set[Agent]):Map[Agent,Set[NPomset]] =
    (for a <- agents yield
      var aBranches:Set[NPomset] = Set()
      for r<-globals
          proja = r.project(a).simplifiedFull
          if !aBranches.exists(p=>areIsomorphic(p,proja).isDefined)
      do aBranches +=proja
      a->aBranches).toMap

  def getTuples(branches:Map[Agent,Set[NPomset]]): Set[List[(Agent,NPomset)]] =
    Utils.crossProduct(branches.map(kv=>toPair(Map(kv)).toList).toList).toSet

  def getAllLocalPrefixes(localBranches:Map[Agent,Set[NPomset]]):Map[Agent,Set[NPomset]] =
    for (a,branches) <- localBranches yield
      a -> (for b <- branches yield prefixes(b)).flatten

  def pp(r:CCPomRes):String =
      r.map(l=>pp(l)).mkString("\n")

  def ppcc2(r:CCPomRes):String =
    (if r.forall(l=>l._2.isDefined) then "CC2 is satisfied\n"
    else "CC2 is not satisfied\n") ++ pp(r)

  def ppcc3(r:CCPomRes):String =
    (if r.forall(l=>l._2.isDefined) then "CC3 is satisfied\n"
    else "CC3 is not satisfied\n") ++ pp(r)

  def pp(l:CCPomLocalRes):String =
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



