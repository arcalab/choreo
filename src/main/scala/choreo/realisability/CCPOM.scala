package choreo.realisability

import choreo.Utils
import choreo.datastructures.Isomorphism.{IsoResult, Isomorphisms}
import choreo.syntax.Agent
import choreo.npomsets._
import choreo.npomsets.NPomset._
import choreo.npomsets.NPomDAG._
import choreo.datastructures._
import choreo.realisability.{ICPOM,CC,ICNPOM}
import CC._

/**
 * Created by guillecledou on 12/07/2021
 */

object CCPOM:

  /**
   * CC2 over NPomsets treated as Pomsets
   *
   * Roberto Guanciale, Emilio Tuosto Interclosure
   * https://doi.org/10.1016/j.jlamp.2019.06.003
   * @param p nested pomset
   * @return cc2 check result with some statistics
   */
  def cc2(p:NPomset): CCPomInfo = //CCPomRes =
    //val refinments = p.refinements
    //(for ic<-IC(p) yield ccpomExists(ic,refinments)).toSet
    val globalPoms  = p.refinements
    val localPoms   = CC.getAllLocalBranches(globalPoms,p.agents)
    val tuples      = CC.getTuples(localPoms)
    val ics         = (for t<-tuples ; ics<- ICPOM(t.toMap)(using true) yield ics).flatten
    val res         = for ic<-ics yield CC.findISO(ic,globalPoms)
    val stats       = CCPOMStats(globalPoms.size,
      localPoms.map(p=>p._1->p._2.size),
      tuples.size,
      ics.size,
      res.count(_._2.isDefined))
    CCPomInfo(stats,res)

  /**
   * CC3 over NPomsets treated as Pomsets
   *
   * Roberto Guanciale, Emilio Tuosto Interclosure
   * https://doi.org/10.1016/j.jlamp.2019.06.003
   * @param p nested pomset
   * @return cc3 check result with some statistics
   */
  def cc3(p:NPomset):CCPomInfo =
    val globalPomsets   = p.refinements
    val globalPrefixes  = (for g<-globalPomsets yield prefixes(g)).flatten
    val localBranches   = CC.getAllLocalBranches(globalPomsets,p.agents)
    val localPrefixes   = CC.getAllLocalPrefixes(localBranches)
    val tuples          = CC.getTuples(localPrefixes)
    val ics             = (for t<-tuples; ics<-ICPOM(t.toMap)(using false) yield ics).flatten
    val res             = for (ic<-ics) yield CC.findISO(ic,globalPrefixes)
    val stats           = CCPOMStats(globalPrefixes.size,
      localPrefixes.map(p=>p._1->p._2.size),
      tuples.size,
      ics.size,
      res.count(_._2.isDefined))
    CCPomInfo(stats,res)