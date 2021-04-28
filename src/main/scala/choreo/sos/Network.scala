package choreo.sos

import choreo.common.{Multiset, Simplify}
import choreo.projection.Projection
import choreo.sos.SOS._
import choreo.sos._
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

case class Network[S](proj:Set[S], pending:Multiset[Action]):
  override def toString: String =
    s"${proj.mkString("  ---  ")}  ${
      if pending.isEmpty then "" else s"  ---  [pending:$pending]"
    }" //[${netw}]"



object Network:
  /** Default constructor with empty network. */
  def apply[S](proj:Set[S]): Network[S] = Network(proj,Multiset())

  /** Default constructor with empty network using a given projection */
  def apply[A,S](s:S,p:Projection[A,S]): Network[S] = Network(p.allProj(s),Multiset())

  def sos[S](localSOS:SOS[Action,S]): SOS[Action,Network[S]] =
    new SOS[Action,Network[S]]:
      override def next(l: Network[S]): Set[(Action, Network[S])] =
        Network.next(localSOS,l.proj,l.pending).map(p=>(p._1,Network(p._2,p._3)))
      override def accepting(l: Network[S]): Boolean =
        l.pending.isEmpty && l.proj.forall(c => SOS.taus(localSOS,c).exists(localSOS.accepting))


  type MActions = Multiset[Action]

  private def next[S](sos:SOS[Action,S],proj:Set[S], netw:MActions): Set[(Action,Set[S],MActions)] =
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






