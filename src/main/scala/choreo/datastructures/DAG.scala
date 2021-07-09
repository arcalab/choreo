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

  /**
   * Sub DAG that contains only nodes in ns and
   * links between those nodes
   * @param ns set of nodes
   * @return sub DAG
   */
  def subDAG(ns:Set[N]):DAG[N] =
    DAG(nodes.intersect(ns),subOrder(ns,successors))

  /**
   * All DAGs that are prefixes of this DAG
    * @return set of prefixes
   */
  def prefixDAGs():Set[DAG[N]] =
    for ns <- this.prefixNodes() yield this.subDAG(ns)

  /**
   * All set of nodes that are prefixes of this DAG
   * @return set of prefixes
   */
  def prefixNodes():Set[Set[N]] =
    processPrefixes(Set(Set()),Set())

  /**
   * Calculates all set of nodes that are prefixes of this DAG
   * @return set of prefixes
   */
  protected def processPrefixes(toProcess:Set[Set[N]],
                                prefixes:Set[Set[N]]):Set[Set[N]] =
    if toProcess.isEmpty then
      prefixes
    else
      val prefix = toProcess.head
      var nToProcess = toProcess - prefix
      for n<-nodes.diff(prefix)
          if pred(n).diff(prefix).isEmpty
      do nToProcess+= prefix + n
      processPrefixes(nToProcess,prefixes+prefix)


object DAG:

  def fromPred[N](nodes:Set[N],predecc:MS[N,N]):DAG[N] =
    val succ = invert(predecc)
    new DAG[N](nodes,succ) {
      override lazy val predecessors: MS[N, N] = predecc
    }

  def fromSucc[N](nodes:Set[N],succ:MS[N,N]):DAG[N] = DAG(nodes,succ)