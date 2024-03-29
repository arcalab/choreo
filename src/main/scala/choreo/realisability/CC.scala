package choreo.realisability

import choreo.Utils
import choreo.common.MRel.asPairs
import choreo.datastructures.Isomorphism
import choreo.datastructures.Isomorphism.Isomorphisms
import choreo.npomsets.NPomDAG.{areIsomorphic, prefixes}
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Event}
import choreo.syntax.Agent

/**
 * Created by guillecledou on 12/07/2021
 */

object CC :

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
    override def toString: String = stats.toString ++ "\n" ++ CC.pp(result)

  def findISO(local:Interclosure, global:List[NPomset])(using simplify:Boolean=false): CCPomLocalRes =
    val localPom = if simplify then local.getNPom.simplifyChoices else local.getNPom.simplifiedFull
    global.view.map(g=>(g,areIsomorphic(g,localPom))).find(_._2.isDefined) match
      case Some((g,Some(iso))) => (local,Some((g,iso)))
      case _ => (local,None)
  //val res = for g <- global yield (g,areIsomorphic(g,localPom))
  //res.find(r=>r._2.isDefined) match
  //  case Some((g,Some(iso))) => (local,Some((g,iso)))
  //  case _ => (local,None)

  def getAllLocalBranches(globals:List[NPomset],agents:Set[Agent])(using simplify:Boolean = false):Map[Agent,Set[NPomset]] =
    (for a <- agents yield
      var aBranches:Set[NPomset] = Set()
      for r<-globals
          proja = if simplify then r.project(a).simplifyChoices else r.project(a).simplifiedFull
          if !aBranches.exists(p=>areIsomorphic(p,proja).isDefined)
      do aBranches += proja
        a->aBranches).toMap

  def getTuples(branches:Map[Agent,Set[NPomset]]): Set[List[(Agent,NPomset)]] =
    Utils.crossProduct(branches.map(kv=>asPairs(using Map(kv)).toList).toList).toSet

  def getAllLocalPrefixes(localBranches:Map[Agent,Set[NPomset]]):Map[Agent,Set[NPomset]] =
    for (a,branches) <- localBranches yield
      a -> (for b <- branches yield prefixes(b)).flatten

  def pp(r:CCPomRes):String =
    "Results by Interclosure:\n\n" ++ r.map(l=>pp(l)).mkString("\n")

  def iscc2(r:CCPomInfo):Boolean =
    !(r.result.isEmpty || (r.stats.interclosures != r.stats.satisfiedIC))

  def ppcc2(r:CCPomInfo):String =
    (if iscc2(r) then "CC2 is satisfied\n"
    else "CC2 is not satisfied\n") ++ r.toString

  def iscc3(r:CCPomInfo):Boolean =
    !(r.result.isEmpty || (r.stats.interclosures != r.stats.satisfiedIC))

  def ppcc3(r:CCPomInfo):String =
    (if iscc3(r) then "CC3 is satisfied\n"
    else "CC3 is not satisfied\n") ++ r.toString


  def pp(l:CCPomLocalRes):String =
    if l._2.isDefined then
      s"""- Local pomset: ${l._1.getNPom}
         |- Global pomset ${l._2.get._1}
         |are isomorphic by the following isomorphism
         |${Isomorphism.ppIsos(l._2.get._2)}
         |""".stripMargin
    else
      s"""- Local pomset: ${l._1.getNPom}
         |is not isomorphic to any global pomset
         |""".stripMargin