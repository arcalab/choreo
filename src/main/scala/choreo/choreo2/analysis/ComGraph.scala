package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.ComGraph._

case class ComGraph(vs:Set[CGVertix], es:Set[CGEdge]):
  def merge(g:ComGraph):ComGraph =
    ComGraph(vs++g.vs, es++g.es)

object ComGraph:
  type CGVertix = Agent
  type CGEdge = (Agent,Agent)

  def comGraphs(c:Choreo):Set[ComGraph] = c match
    case Send(as, bs, m) => Set(ComGraph(as.toSet++bs,as.flatMap(a=> bs.map(b=> (a,b))).toSet))
    case Seq(c1, c2) => comGraphs(c1).flatMap(g1 =>comGraphs(c2).map(g2=> g1.merge(g2)))
    case Par(c1, c2) => comGraphs(c1).flatMap(g1 =>comGraphs(c2).map(g2=> g1.merge(g2)))
    case Choice(c1, c2) => comGraphs(c1) ++ (comGraphs(c2))
    case Loop(c) => comGraphs(c)
    case Out(a,b,_) => Set(ComGraph(Set(a,b),Set((a,b))))
    case _ => Set() // End, Out

  def comGraphsPP(c:Choreo):String =
    comGraphs(c).map(comGraphPP).mkString("\n-----\n")

  def comGraphPP(g:ComGraph):String =
    g.es.groupBy(_._1).map(a=> a._1.s ++ " -> " ++ a._2.map(_._2.s).mkString(",")).mkString("\n")
