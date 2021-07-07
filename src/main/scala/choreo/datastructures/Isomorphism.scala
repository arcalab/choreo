package choreo.datastructures

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{MS, invert}

/**
 * Created by guillecledou on 06/07/2021
 */

object Isomorphism:

  /**
   * Checks if two DAGs are isomorphic.
   * Uses the algorithm described in:
   * "An efficient algorithm for the inexact matching of ARG graphs using a contextual transformational model"
   * It can be improved as described in:
   * "An Improved Algorithm for Matching Large Graphs"
   * @param g1 DAG
   * @param g2 DAG
   * @param f comparison function that checks if two nodes are semantically the same
   * @tparam N1 type of the nodes in g1
   * @tparam N2 type of the ndoes in g2
   * @return a set of possible isomorphism if they exist
   */
  def isIsomorphic[N1,N2](g1:Graph[N1],g2:Graph[N2],f:((N1,N2))=>Boolean):Option[Set[PS[N1,N2]]] =
    var k:Int = 0
    var all:Set[PS[N1,N2]] = Set(PS()) // all partial solutions in the current iteration
    while
      var nall:Set[PS[N1,N2]] = Set()
      for (s<-all)
        val npairs = expandPS(s,g1,g2,f)
        if npairs.nonEmpty then nall ++= npairs
      k+=1
      all = nall //if nall.nonEmpty then all = nall
      (g1.nodes.size != k) && nall.nonEmpty
    do ()
    if all.nonEmpty && k == g1.nodes.size then Some(all) else None

  protected def expandPS[N1,N2](ps:PS[N1,N2],g1:Graph[N1],g2:Graph[N2],f:((N1,N2))=>Boolean):Set[PS[N1,N2]] =
    val t = PSOI(ps.inLeft(g1),ps.outLeft(g1),ps.inRight(g2),ps.outRight(g2))
    val npairs = pairs(ps,g1,g2)
    val vpairs = npairs.filter(p=>f(p) && syntacticCheck(p,ps,g1,g2,t))
    vpairs.map(p=>ps.add(p))

  protected def pairs[N1,N2](ps:PS[N1,N2],g1:Graph[N1],g2:Graph[N2]):Set[(N1,N2)] =
    lazy val pairsOut = for n1 <- ps.outLeft(g1); n2<- ps.outRight(g2) yield (n1,n2)
    lazy val pairsIn  = for n1 <- ps.inLeft(g1); n2 <- ps.inRight(g2) yield (n1,n2)
    lazy val pairsD   = for n1 <- g1.nodes -- ps.left; n2 <- g2.nodes -- ps.right yield (n1,n2)
    if pairsOut.nonEmpty then pairsOut
    else if pairsIn.nonEmpty then pairsIn
    else pairsD

  /////////////////////////////////////
  // Syntactic check
  /////////////////////////////////////

  protected def syntacticCheck[N1,N2](pair:(N1,N2), ps:PS[N1,N2],
                                      g1:Graph[N1], g2:Graph[N2], t:PSOI[N1,N2]):Boolean =
    rpred(pair,ps,g1,g2)
      && rsucc(pair,ps,g1,g2)
      && rin(pair,ps,g1,g2,t)
      && rout(pair,ps,g1,g2,t)
      && rnew(pair,ps,g1,g2,t)

  protected def rpred[N1,N2](pair:(N1,N2), ps:PS[N1,N2], g1:Graph[N1], g2:Graph[N2]):Boolean =
    val (n,m) = pair
    val predn = g1.pred(n)
    val predm = g2.pred(m)
    ps.left.intersect(predn).forall(n1=>predm.intersect(ps.succ(n1)).nonEmpty) &&
      ps.right.intersect(predm).forall(m1=>predn.intersect(ps.pred(m1)).nonEmpty)

  protected def rsucc[N1,N2](pair:(N1,N2), ps:PS[N1,N2], g1:Graph[N1], g2:Graph[N2]):Boolean =
    val (n,m) = pair
    val succn = g1.succ(n)
    val succm = g2.succ(m)
    ps.left.intersect(succn).forall(n1=>succm.intersect(ps.succ(n1)).nonEmpty) &&
      ps.right.intersect(succm).forall(m1=>succn.intersect(ps.pred(m1)).nonEmpty)

  protected def rin[N1,N2](pair:(N1,N2), ps:PS[N1,N2],
                           g1:Graph[N1], g2:Graph[N2], t:PSOI[N1,N2]):Boolean =
    val (n,m) = pair
    g1.succ(n).intersect(t.t1in).size == g2.succ(m).intersect(t.t2in).size &&
      g1.pred(n).intersect(t.t1in).size == g2.pred(m).intersect(t.t2in).size

  protected def rout[N1,N2](pair:(N1,N2), ps:PS[N1,N2],
                            g1:Graph[N1], g2:Graph[N2], t:PSOI[N1,N2]):Boolean =
    val (n,m) = pair
    g1.pred(n).intersect(t.t1out).size == g2.pred(m).intersect(t.t2out).size &&
      g1.succ(n).intersect(t.t1out).size == g2.succ(m).intersect(t.t2out).size

  protected def rnew[N1,N2](pair:(N1,N2), ps:PS[N1,N2],
                            g1:Graph[N1], g2:Graph[N2], t:PSOI[N1,N2]):Boolean =
    val (n,m) = pair
    val set1 = g1.nodes -- ps.left -- (t.t1in++t.t1out)
    val set2 = g2.nodes -- ps.right -- (t.t2in++t.t2out)
    g1.pred(n).intersect(set1).size == g2.pred(m).intersect(set2).size &&
      g1.succ(n).intersect(set1).size == g2.succ(m).intersect(set2).size

  /////////////////////////////////////
  // partial solution state
  /////////////////////////////////////

  case class PS[N1,N2](pred:MS[N2,N1],succ:MS[N1,N2]):
    lazy val left:Set[N1] = succ.keySet
    lazy val right:Set[N2] = pred.keySet

    def add(edge:(N1,N2)):PS[N1,N2] =
      val (n1,n2) = edge
      PS(NPomset.add((n2,n1),pred),NPomset.add(edge,succ))

    def outLeft(g:Graph[N1]):Set[N1] =
      (g.nodes -- left).filter(n => g.succ(n).intersect(left).nonEmpty)
    def outRight(g:Graph[N2]):Set[N2] =
      (g.nodes -- right).filter(n => g.succ(n).intersect(right).nonEmpty)
    def inLeft(g:Graph[N1]):Set[N1] =
      (g.nodes -- left).filter(n => g.pred(n).intersect(left).nonEmpty)
    def inRight(g:Graph[N2]):Set[N2] =
      (g.nodes -- right).filter(n => g.pred(n).intersect(right).nonEmpty)

    override def toString:String =
      succ.flatMap({case (from,tos) => tos.map(to=> s"($from,$to)")}).mkString(",")

  /**
   * Auxiliary class.
   * Nodes from the graphs being compared that have incomming and outgoing nodes in a partial solution
   * @param t1in incomming nodes to left graph
   * @param t1out outgoing nodes to left graph
   * @param t2in incomming nodes to right graph
   * @param t2out outgoing nodes to right graph
   * @tparam N1 type of the nodes of left graph
   * @tparam N2 type of the nodes of right graph
   */
  case class PSOI[N1,N2](t1in:Set[N1],t1out:Set[N1],t2in:Set[N2],t2out:Set[N2])

  object PS:
    def apply[N1,N2]():PS[N1,N2] = PS(Map(),Map())
