package choreo.sos

import choreo.common.Simplify
import choreo.projection.Projection
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Choreo, Msg}
import caos.common.Multiset
import caos.sos.SOS.*
import caos.sos.*
import choreo.sos.ChorSyncSOS.Interact
import choreo.sos.Network.NetworkCausal.Queue

private type Act = Choreo


object Network:

  ///////////////////
  // Multi-set-based network (sending order not preserved)
  ///////////////////
  /** Default constructor with empty network. */
  def mkNetMS[S](proj:Set[S]): NetworkMS[S] = NetworkMS(proj,Multiset())

  /** Default constructor with empty network using a given projection */
  def mkNetMS[A,S](s:S,p:Projection[A,S]): NetworkMS[S] = NetworkMS(p.allProj(s),Multiset())

  /** Produces an SOS object (with next and accepting) for networks */
  def sosMS[S](localSOS:SOS[Act,S]): SOS[Act,NetworkMS[S]] =
    new SOS[Act,NetworkMS[S]]:
      override def next[A>:Act](l: NetworkMS[S]): Set[(A, NetworkMS[S])] =
        NetworkMS.next(localSOS,l.proj,l.pending).map(p=>(p._1,NetworkMS(p._2,p._3)))
      override def accepting(l: NetworkMS[S]): Boolean =
//        l.pending.isEmpty &&
        l.proj.forall(c => SOS.byTau(localSOS,c).exists(localSOS.accepting))

  /** Network of a set of terms together with a multiset of pending actions. */
  case class NetworkMS[S](proj:Set[S], pending:Multiset[Act]):
    override def toString: String =
      s"${proj.mkString("  ---  ")}  ${
        if pending.isEmpty then "" else s"  ---  [pending:$pending]"
      }" //[${netw}]"

  object NetworkMS:
    type MActions = Multiset[Act]

    def next[S](sos:SOS[Act,S],proj:Set[S], netw:MActions): Set[(Act,Set[S],MActions)] =
      val x = for (p <- proj) yield  // get each projection
        val proj2 = evolveProj(sos,p,netw) // get all evolutions = (act,newProj,newNet)
        val newProj = for (act,p2,n2)<-proj2 yield
          (act, proj-p+p2 , n2 )
        newProj
      x.flatten

    private def evolveProj[S](sos:SOS[Act,S],c:S, net:MActions): Set[(Act,S,MActions)] =
      for (act,chor)<-sos.next(c) if allowed(act,net) yield
        (act,chor, updateNet(act,net))
    private def updateNet(act:Act, net:MActions): MActions = act match
      case Seq(c1, c2) => updateNet(c2, updateNet(c1, net))
      case Par(c1, c2) => updateNet(c2, updateNet(c1, net))
      case In(a, b, m) => net - Out(b, a, m)
      case Out(a, b, m) => net + Out(a, b, m)
      case Tau | Internal(_, _) => net
      case _ => sys.error(s"Unsupported action $act when evolving a projection")

    private def allowed(act: Act, net: MActions): Boolean =
      act match
        case Seq(c1, c2) => allowed(c1, net) && allowed(c2, updateNet(c2, net))
        case Par(c1, c2) => allowed(c1, net) && allowed(c2, net)
        case In(a, b, m) => net contains Out(b,a,m)
        case Out(_, _, _) => true
        case Internal(_, _) => true
        case Tau => true
        case _ => sys.error(s"Unsupported action $act when evolving a projection")


  ///////////////////
  // List-based network (message-order preserving)
  ///////////////////

  def mkNetCS[S](proj:Set[S]): NetworkCausal[S] = NetworkCausal(proj,Map())
  def mkNetCS[A,S](s:S,p:Projection[A,S]): NetworkCausal[S] = NetworkCausal(p.allProj(s),Map())

  /** Produces an SOS object (with next and accepting) for networks */
  def sosCS[S](localSOS:SOS[Act,S]): SOS[Act,NetworkCausal[S]] =
    new SOS[Act,NetworkCausal[S]]:
      override def next[A>:Act](l: NetworkCausal[S]): Set[(A, NetworkCausal[S])] =
        NetworkCausal.next(localSOS,l.proj,l.pending).map(p=>(p._1,NetworkCausal(p._2,p._3)))
      override def accepting(l: NetworkCausal[S]): Boolean =
      //        l.pending.isEmpty &&
        l.proj.forall(c => SOS.byTau(localSOS,c).exists(localSOS.accepting))

  case class NetworkCausal[S](proj:Set[S], pending: Queue):
    override def toString: String =
      s"${proj.mkString("  ---  ")}  ${
        if pending.isEmpty then "" else s"  ---  [pending:${
          pending.map(x=>s"${x._1._1}-${x._1._2}->${x._2.map(_.names).mkString(",")}").mkString("; ")}]"
      }" //[${netw}]"

  object NetworkCausal:
    type Queue = Map[(Agent,Agent),List[Msg]]

    def next[S](sos:SOS[Act,S],proj:Set[S], netw:Queue): Set[(Act,Set[S],Queue)] =
      val x = for (p <- proj) yield  // get each projection
        val proj2 = evolveProj(sos,p,netw) // get all evolutions = (act,newProj,newNet)
        val newProj = for (act,p2,n2)<-proj2 yield
          (act, proj-p+p2 , n2 )
        newProj
      x.flatten

    private def evolveProj[S](sos:SOS[Act,S],c:S, net:Queue): Set[(Act,S,Queue)] =
      for (act,chor)<-sos.next(c) if allowed(act,net) yield
        (act,chor, updateNet(act,net))
    private def updateNet(a:Act,net:Queue): Queue = a match
      case Seq(c1,c2) => updateNet(c2,updateNet(c1,net))
      case Par(c1,c2) => updateNet(c2,updateNet(c1,net))
      case In(a, b, _) => net + ((b, a) -> net((b, a)).tail) // take out the tail
      case Out(a, b, m) => net + ((a, b) -> (net.getOrElse((a, b), Nil) ::: List(m))) // add to the end
      case Tau | Internal(_, _) => net
      case _ => sys.error(s"Unsupported action $a when evolving a projection")

    private def allowed(act: Act, net: Queue): Boolean =
      act match
        case Seq(c1, c2) => allowed(c1,net) && allowed(c2,updateNet(c2,net))
        case Par(c1, c2) => allowed(c1,net) && allowed(c2,net)
        case In(a, b, m) =>
          net.contains((b,a)) && net((b,a)).nonEmpty && net((b,a)).head == m //net contains b,a->m::...
        case Out(_, _, _) => true
        case Internal(_, _) => true
        case Tau => true
        case _ => sys.error(s"Unsupported action $act when evolving a projection")


  ///////////////////
  // Network of Team Automata (multi-synchronous communication)
  ///////////////////

  /** A synchronisation type maps messages to the
   * possible number of sending receiving agents. */
  type SyncType = Map[Msg,(Set[Int],Set[Int])]
  // Only works with labels given by Choreo expressions (typically Sends)
  def mkNetSync[S](s: S, p: Projection[Agent, S], st:SyncType): NetworkSync[S] =
    NetworkSync(p.allAProj(s), st)

  case class NetworkSync[S](proj:Set[(Agent,S)],st:SyncType):
    override def toString: String =
      proj.map(kv=>s"${kv._1}: ${kv._2}").mkString("\n")

  def sosSync[S](localSOS:SOS[Interact,S]): SOS[Interact,NetworkSync[S]] =
    new SOS[Interact,NetworkSync[S]]:
      override def next[A>:Interact](l: NetworkSync[S]): Set[(A, NetworkSync[S])] =
        nextAux(localSOS,l.proj.toMap,l.st)
          .toSet
          .map(p=>(p._1,NetworkSync(p._2,l.st)))
      override def accepting(l: NetworkSync[S]): Boolean =
      //        l.pending.isEmpty &&
        l.proj.forall(c => SOS.byTau(localSOS,c._2).exists(localSOS.accepting))

      private def nextAux[A>:Interact](l:SOS[Interact,S],proj:Map[Agent,S],st:SyncType): Set[(A,Set[(Agent,S)])] =
        var msgs = Map[Msg,(Set[(Agent,S)],Set[(Agent,S)])]() // map messages to input and ouput agents that can evolve by it
        for (ag,s1) <- proj; (Interact(as,bs,m),s2) <- l.next(s1) do
          val (ins,outs) = msgs.getOrElse(m,Set()->Set())
          val ins2  = if as.contains(ag) then ins+(ag->s2)  else ins
          val outs2 = if bs.contains(ag) then outs+(ag->s2) else outs
          msgs += m -> (ins2,outs2)
        //println(s"from [${proj.mkString(",")}] I can do [${msgs.mkString(",")}]")
        val intern =
          for
            (m, (is, os)) <- msgs.toSet
            if !st.contains(m) // && is.size==1
            i <- is
          yield
            updSt[A](proj, m, Set(i), Set(i))
        val comm =
          for
            (m,(ins,outs)) <-msgs.toSet
            is<-ps(ins)
            os<-ps(outs)
            if // (is.nonEmpty||os.nonEmpty) //&&
              st.contains(m) && (
                st(m)._1.contains(is.size) &&
                st(m)._2.contains(os.size))
          yield
            updSt[A](proj,m,is,os)
        comm++intern

      private def ps[A](s:Set[A]): Set[Set[A]] = s.headOption match
          case None => Set(Set())
          case Some(e) => (ps(s-e).map(_+e)++ps(s-e))+s

      private def updSt[A>:Interact](proj:Map[Agent,S],m:Msg,is:Set[(Agent,S)],os:Set[(Agent,S)])
          : (A,Set[(Agent,S)]) =
        Interact(is.map(_._1),os.map(_._1),m) -> (proj ++ (for (a,s)<-is++os yield (a->s))).toSet






