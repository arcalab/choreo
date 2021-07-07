package choreo.datastructures

import choreo.npomsets.NPomset._
import Graph._
import choreo.npomsets.NPomset

/**
 * Created by guillecledou on 06/07/2021
 */

case class Graph[N](nodes:Set[N], successors:MS[N,N]):
  lazy val predecessors:MS[N,N] = invert(successors)
  def pred(n:N):Set[N] = predecessors.getOrElse(n,Set())
  def succ(n:N):Set[N] = successors.getOrElse(n,Set())
  override def toString:String =
    s"""nodes: ${nodes.mkString(",")}
       |order: ${successors.flatMap({case (from,tos) => tos.map(to=> s"($from,$to)")}).mkString(",")}
       |""".stripMargin


object Graph:

  def fromPred[N](nodes:Set[N],predecc:MS[N,N]):Graph[N] =
    val succ = invert(predecc)
    new Graph[N](nodes,succ) {
      override lazy val predecessors: MS[N, N] = predecc
    }

  def fromSucc[N](nodes:Set[N],succ:MS[N,N]):Graph[N] = Graph(nodes,succ)