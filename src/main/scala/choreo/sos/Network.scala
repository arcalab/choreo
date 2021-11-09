package choreo.sos

import choreo.common.Simplify
import choreo.projection.Projection
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Choreo, Msg}
import caos.common.Multiset
import caos.sos.SOS.*
import caos.sos.*
import choreo.sos.Network.NetworkCausal.Queue


object Network:
  /** Default constructor with empty network. */
  def mkNetMS[S](proj:Set[S]): NetworkMS[S] = NetworkMS(proj,Multiset())

  /** Default constructor with empty network using a given projection */
  def mkNetMS[A,S](s:S,p:Projection[A,S]): NetworkMS[S] = NetworkMS(p.allProj(s),Multiset())

  def mkNetCS[S](proj:Set[S]): NetworkCausal[S] = NetworkCausal(proj,Map())
  def mkNetCS[A,S](s:S,p:Projection[A,S]): NetworkCausal[S] = NetworkCausal(p.allProj(s),Map())

  /** Produces an SOS object (with next and accepting) for networks */
  def sosMS[S](localSOS:SOS[Action,S]): SOS[Action,NetworkMS[S]] =
    new SOS[Action,NetworkMS[S]]:
      override def next(l: NetworkMS[S]): Set[(Action, NetworkMS[S])] =
        NetworkMS.next(localSOS,l.proj,l.pending).map(p=>(p._1,NetworkMS(p._2,p._3)))
      override def accepting(l: NetworkMS[S]): Boolean =
//        l.pending.isEmpty &&
        l.proj.forall(c => SOS.byTau(localSOS,c).exists(localSOS.accepting))

  /** Produces an SOS object (with next and accepting) for networks */
  def sosCS[S](localSOS:SOS[Action,S]): SOS[Action,NetworkCausal[S]] =
    new SOS[Action,NetworkCausal[S]]:
      override def next(l: NetworkCausal[S]): Set[(Action, NetworkCausal[S])] =
        NetworkCausal.next(localSOS,l.proj,l.pending).map(p=>(p._1,NetworkCausal(p._2,p._3)))
      override def accepting(l: NetworkCausal[S]): Boolean =
      //        l.pending.isEmpty &&
        l.proj.forall(c => SOS.byTau(localSOS,c).exists(localSOS.accepting))


  //////////////////////////////

  case class NetworkCausal[S](proj:Set[S], pending: Queue):
    override def toString: String =
      s"${proj.mkString("  ---  ")}  ${
        if pending.isEmpty then "" else s"  ---  [pending:${
          pending.map(x=>s"${x._1._1}-${x._1._2}->${x._2.map(_.names).mkString(",")}").mkString("; ")}]"
      }" //[${netw}]"

  object NetworkCausal:
    type Queue = Map[(Agent,Agent),List[Msg]]

    def next[S](sos:SOS[Action,S],proj:Set[S], netw:Queue): Set[(Action,Set[S],Queue)] =
      val x = for (p <- proj) yield  // get each projection
        val proj2 = evolveProj(sos,p,netw) // get all evolutions = (act,newProj,newNet)
        val newProj = for (act,p2,n2)<-proj2 yield
          (act, proj-p+p2 , n2 )
        newProj
      x.flatten

    private def evolveProj[S](sos:SOS[Action,S],c:S, net:Queue): Set[(Action,S,Queue)] =
      for (act,chor)<-sos.next(c) if allowed(act,net) yield
        (act,chor, act match
          case In(a,b,m)  => net + ((b,a) -> net((b,a)).tail) // take out the tail
          case Out(a,b,m) => net + ((a,b) -> (net.getOrElse((a,b),Nil):::List(m))) // add to the end
          case Tau => net
        )
    private def allowed(act: Action, net: Queue): Boolean =
      val res = act match
        case In(a, b, m) =>
          net.contains((b,a)) && net((b,a)).nonEmpty && net((b,a)).head == m //net contains b,a->m::...
        case Out(_, _, _) => true
        case Tau => true
      res


  /** Network of a set of terms together with a multiset of pending actions. */
  case class NetworkMS[S](proj:Set[S], pending:Multiset[Action]):
    override def toString: String =
      s"${proj.mkString("  ---  ")}  ${
        if pending.isEmpty then "" else s"  ---  [pending:$pending]"
      }" //[${netw}]"

  object NetworkMS:
    type MActions = Multiset[Action]

    def next[S](sos:SOS[Action,S],proj:Set[S], netw:MActions): Set[(Action,Set[S],MActions)] =
      val x = for (p <- proj) yield  // get each projection
        val proj2 = evolveProj(sos,p,netw) // get all evolutions = (act,newProj,newNet)
        val newProj = for (act,p2,n2)<-proj2 yield
          (act, proj-p+p2 , n2 )
        newProj
      x.flatten

    private def evolveProj[S](sos:SOS[Action,S],c:S, net:MActions): Set[(Action,S,MActions)] =
      for (act,chor)<-sos.next(c) if allowed(act,net) yield
        (act,chor, act match
          case In(a,b,m)  => net - Out(b,a,m)
          case Out(a,b,m) => net + Out(a,b,m)
          case Tau => net
        )

    private def allowed(act: Action, net: MActions): Boolean =
      act match
        case In(a, b, m) => net contains Out(b,a,m)
        case Out(_, _, _) => true
        case Tau => true






