package choreo.analysis

import choreo.analysis.ComGraph._
import choreo.syntax.Choreo._
import choreo.syntax._

object Bounded :

  /** Checks if all bodies of loops are strongly connected, by traversing their communication graphs. */
  def boundedChoreo(c:Choreo):Boolean = c match
    case Seq(c1, c2) => boundedChoreo(c1) && boundedChoreo(c2)
    case Par(c1, c2) => boundedChoreo(c1) && boundedChoreo(c2)    // todo: not sure yet
    case Choice(c1, c2) => boundedChoreo(c1) && boundedChoreo(c2)
    case DChoice(c1, c2) => boundedChoreo(c1) && boundedChoreo(c2)
    case Loop(c) => comGraphs(c).forall(stronglyConnected) && boundedChoreo(c) // todo: not sure
    case _ => true // Send,End,Action

  // naive implementation
  /** Checks if a communication graph is strongly connected. */
  def stronglyConnected(g:ComGraph):Boolean =
    lazy val a = g.vs.head
    lazy val reach = reachableFrom(a,g)
    lazy val reverseReach = reachableFrom(a,ComGraph(g.vs,g.es.map(_.swap)))
    g.vs.isEmpty || (reach == g.vs && reverseReach == g.vs)

  private def reachableFrom(v:CGVertix,g:ComGraph):Set[CGVertix] =
    var edges = g.es.groupBy(_._1).map(v=> v._1 -> v._2.map(_._2))
    var visited = Set(v)
    var toVisit = edges.getOrElse(v,Set())
    while toVisit.nonEmpty && visited!= g.vs do
      val next = toVisit.head
      toVisit ++= edges.getOrElse(next,Set()) -- visited
      toVisit -= next
      visited += next
    visited