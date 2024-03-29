package choreo.datastructures

import choreo.npomsets.NPomset._
import choreo.npomsets.NPomset
import choreo.common.MRel._
import choreo.datastructures.DAG
import DAG._
import choreo.datastructures.Isomorphism.IsoResult


/**
 * Created by guillecledou on 06/07/2021
 */

case class DAG[N](nodes:Set[N], edges:MR[N,N]):
  lazy val predecessors:MR[N,N] = invert(using edges)

  def pred(n:N):Set[N] = predecessors.getOrElse(n,Set())
  def succ(n:N):Set[N] = edges.getOrElse(n,Set())

  def +(edge:(N,N)):DAG[N] =
    DAG(nodes+edge._1+edge._2,(edges :+ edge))

  def ++(succ:MR[N,N]):DAG[N] =
    DAG[N](nodes++succ.keySet++succ.values.flatten,(edges :++ succ))

  def transitiveClosure:DAG[N] = DAG(nodes,closure(nodes)(using edges))

  def transitiveReduction:DAG[N] = DAG(nodes,reduction(nodes)(using edges))

  /**
   * Sub DAG that contains only nodes in ns and
   * links between those nodes
   * @param ns set of nodes
   * @return sub DAG
   */
  def subDAG(ns:Set[N]):DAG[N] =
    val cl = this.transitiveClosure
    //val ne = cl.edges.collect({case (k,v) if ns.contains(k)=> (k,v.intersect(ns))})
    DAG(nodes.intersect(ns),cl.edges).transitiveReduction

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
  //processPrefixes(Set(Set()),Set())
    processPrefixes()

  /**
   * Calculates all set of nodes that are prefixes of this DAG
   * @return set of prefixes
   */
  //protected def processPrefixes(toProcess:Set[Set[N]], prefixes:Set[Set[N]]):Set[Set[N]] =
  //  if toProcess.nonEmpty then
  //    val prefix = toProcess.head
  //    var nToProcess = toProcess - prefix
  //    for n<-nodes.diff(prefix)
  //        if pred(n).diff(prefix).isEmpty
  //    do nToProcess+= prefix + n
  //    processPrefixes(nToProcess,prefixes+prefix)
  //  else prefixes
  protected def processPrefixes():Set[Set[N]] =
    var toProcess:Set[Set[N]] = Set(Set())
    var prefixes:Set[Set[N]] = Set()
    while toProcess.nonEmpty do
      val prefix = toProcess.head
      toProcess -= prefix
      for n<-nodes.diff(prefix)
          if pred(n).diff(prefix).isEmpty
      do toProcess+= prefix + n
      prefixes+=prefix
    prefixes

  override def toString:String = asPairs(using edges).mkString(",")
//  //s"""nodes: ${nodes.mkString(",")}
//  //   |order: ${edges.flatMap({case (from,tos) => tos.map(to=> s"($from,$to)")}).mkString(",")}
//  //   |""".stripMargin



object DAG:

  def fromPred[N](nodes:Set[N],predecc:MR[N,N]):DAG[N] =
    val succ = invert(using predecc)
    new DAG[N](nodes,succ) {
      override lazy val predecessors: MR[N, N] = predecc
    }

  def fromSucc[N](nodes:Set[N],succ:MR[N,N]):DAG[N] =
    DAG(nodes,succ)