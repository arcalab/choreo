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
  type CCPomLocalRes = (Interclosure,Option[(NPomset,Isomorphisms[Event,Event])])

  /**
   * Statistics generated during ccpom checks
   *
   * @param globalPomsets number of global pomsets againts which local pomsets must be compare
   * @param localPomsetsPerAgent number of local pomsets per agent
   * @param localTuples number of local tuples obtained by combining local pomsets (one for each agent)
   * @param interclosures number of valid interclosures obtained
   * @param satisfiedIC number of interclosures that are isomorphic to som global pomset
   */
  case class CCPOMStats(globalPomsets:Int,
                        localPomsetsPerAgent:Map[Agent,Int],
                        localTuples:Int,
                        interclosures:Int,
                        satisfiedIC:Int):
    override def toString: String =
      s"""
         |Statistics:
         |- # global pomsets: $globalPomsets
         |- # local pomsets per agent:
         |${localPomsetsPerAgent.map(l=>s"  - ${l._1}: ${l._2}").mkString("\n")}
         |- # tuples: ${localTuples}
         |- # interclosures: ${interclosures}
         |- # KO interclosures: ${interclosures-satisfiedIC}
         |""".stripMargin

  case class CCPomInfo(stats:CCPOMStats,result: CCPomRes):
    override def toString: String = stats.toString ++ "\n" ++ CCPOM.pp(result)

  def cc2(p:NPomset): CCPomInfo = //CCPomRes =
    //val refinments = p.refinements
    //(for ic<-IC(p) yield ccpomExists(ic,refinments)).toSet
    val globalPoms  = p.refinements
    val localPoms   = getAllLocalBranches(globalPoms,p.agents)
    val tuples      = getTuples(localPoms)
    val ics         = (for t<-tuples ; ics<- IC(t.toMap) yield ics).flatten
    val res         = for ic<-ics yield ccpomExists(ic,globalPoms)
    val stats       = CCPOMStats(globalPoms.size,
      localPoms.map(p=>p._1->p._2.size),
      tuples.size,
      ics.size,
      res.count(_._2.isDefined))
    CCPomInfo(stats,res)

  def cc3(p:NPomset):CCPomInfo =
    val globalPomsets   = p.refinements
    val globalPrefixes  = (for g<-globalPomsets yield prefixes(g)).flatten
    val localBranches   = getAllLocalBranches(globalPomsets,p.agents.toSet)
    val localPrefixes   = getAllLocalPrefixes(localBranches)
    val tuples          = getTuples(localPrefixes)
    val ics             = (for t<-tuples; ics<-IC(t.toMap)(using false) yield ics).flatten
    val res             = for (ic<-ics) yield ccpomExists(ic,globalPrefixes)
    val stats           = CCPOMStats(globalPrefixes.size,
      localPrefixes.map(p=>p._1->p._2.size),
      tuples.size,
      ics.size,
      res.count(_._2.isDefined))
    CCPomInfo(stats,res)

  def ccpomExists(local:Interclosure, global:List[NPomset]): CCPomLocalRes =
    val localPom = local.getPom.simplifiedFull
    global.view.map(g=>(g,areIsomorphic(g,localPom))).find(_._2.isDefined) match
      case Some((g,Some(iso))) => (local,Some((g,iso)))
      case _ => (local,None)
  //val res = for g <- global yield (g,areIsomorphic(g,localPom))
  //res.find(r=>r._2.isDefined) match
  //  case Some((g,Some(iso))) => (local,Some((g,iso)))
  //  case _ => (local,None)

  def getAllLocalBranches(globals:List[NPomset],agents:Iterable[Agent]):Map[Agent,Set[NPomset]] =
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
    "Results by Interclosure:\n\n" ++ r.map(l=>pp(l)).mkString("\n")

  def ppcc2(r:CCPomInfo):String =
    (if r.stats.interclosures == r.stats.satisfiedIC then "CC2 is satisfied\n"
    else "CC2 is not satisfied\n") ++ r.toString

  def ppcc3(r:CCPomInfo):String =
    (if r.stats.interclosures == r.stats.satisfiedIC then "CC3 is satisfied\n"
    else "CC3 is not satisfied\n") ++ r.toString

  def pp(l:CCPomLocalRes):String =
    if l._2.isDefined then
      s"""- Local pomset: ${l._1.getPom}
         |- Global pomset ${l._2.get._1}
         |are isomorphic by the following isomorphism
         |${Isomorphism.ppIsos(l._2.get._2)}
         |""".stripMargin
    else
      s"""- Local pomset: ${l._1.getPom}
         |is not isomorphic to any global pomset
         |""".stripMargin
