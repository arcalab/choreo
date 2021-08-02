package choreo.realisability


import choreo.npomsets._
import choreo.npomsets.NPomset._
import choreo.npomsets.NPomDAG._
import choreo.datastructures._
import choreo.realisability.{ICPOM,CC,ICNPOM}
import CC._

object CCNPOM:

  def cc2(p:NPomset):CCPomInfo =
    val globalBranch  = p.simplifyChoices
    val localBranches = CC.getAllLocalBranches(globalBranch::Nil,p.agents)(using true)
    val tuples        = CC.getTuples(localBranches)
    val ics           = (for t<-tuples ; ics <- ICNPOM(t.toMap)(using true) yield ics).flatten
    val res           = for ic<-ics yield CC.findISO(ic,globalBranch::Nil)
    val stats         = CCPOMStats(1,
      localBranches.map(p=>p._1->p._2.size),
      tuples.size,
      ics.size,
      res.count(_._2.isDefined))
    CCPomInfo(stats,res)
