package choreo.datastructures

import choreo.npomsets.NPomset._
import DAG._
import choreo.npomsets.NPomset

/**
 * Created by guillecledou on 06/07/2021
 */

case class DAG[N](nodes:Set[N], successors:MS[N,N]):
  lazy val predecessors:MS[N,N] = invert(successors)
  def pred(n:N):Set[N] = predecessors.getOrElse(n,Set())
  def succ(n:N):Set[N] = successors.getOrElse(n,Set())
  override def toString:String = toPair(successors).mkString(",")
    //s"""nodes: ${nodes.mkString(",")}
    //   |order: ${successors.flatMap({case (from,tos) => tos.map(to=> s"($from,$to)")}).mkString(",")}
    //   |""".stripMargin

  def ++(succ:MS[N,N]):DAG[N] =
    DAG[N](nodes++succ.keySet++succ.values.flatten,add(succ,successors))

object DAG:

  def fromPred[N](nodes:Set[N],predecc:MS[N,N]):DAG[N] =
    val succ = invert(predecc)
    println(s"[fromPred] - predec= $predecc")
    println(s"[fromPred] - succ= $succ")
    println(s"[fromPred] - toPair= ${toPair(succ)}")
    new DAG[N](nodes,succ) {
      override lazy val predecessors: MS[N, N] = predecc
    }

  def fromSucc[N](nodes:Set[N],succ:MS[N,N]):DAG[N] = DAG(nodes,succ)