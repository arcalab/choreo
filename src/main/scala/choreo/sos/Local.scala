package choreo.sos

import choreo.sos.{Local, LocalBasic, LocalManyTaus}
import choreo.common.{Multiset, Simplify}
import choreo.sos.{ChorDefSOS, ChorManyTausSOS, SOS}
import choreo.sos.SOS._
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

@deprecated
case class Local(proj:Set[Choreo], netw:Multiset[Action]):
  override def toString: String =
    s"${proj.mkString("  ---  ")}  ${
      if netw.isEmpty then "" else s"  ---  [pending:$netw]"
    }" //[${netw}]"

//given LTS[Local]:
//  extension(l:Local)
//    def next: Set[(Action,Local)] =
////      Local.next(l)
//      Local.next(l.proj,l.netw).map(p=>(p._1,Local(p._2,p._3)))
//    def accepting: Boolean =
//      l.netw.isEmpty && l.proj.forall(c => taus(c).exists(_.accepting))

@deprecated
object Local extends SOS[Action,Local]:
  override def accepting(l: Local): Boolean =
    l.netw.isEmpty && l.proj.forall(c => SOS.taus(ChorDefSOS,c).exists(ChorDefSOS.accepting))

  override def next(l:Local): Set[(Action,Local)] =
    Local.next(ChorDefSOS,l.proj,l.netw).map(p=>(p._1,Local(p._2,p._3)))

  def apply(c:Choreo): Local =
    Local(allProj(c).map(_._2),Multiset())
    
  def allProj(c:Choreo): Set[(Agent,Choreo)] =
    (for a<-agents(c) yield a->proj(c,a))
  
  /** Projects a Choreo expression into an agent. */
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projAux(c,a))
  //todo: check DChoice
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projAux((outs++ins).fold(End)(_>_) , a)
    case Seq(c1, c2) => projAux(c1,a) > projAux(c2,a)
    case Par(c1, c2) => projAux(c1,a) || projAux(c2,a)
    case Choice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
      case (End,End) => Tau + Tau
      case (c1p,End) => c1p + Tau
      case (End,c2p) => Tau + c2p
      case (c1p,c2p) => c1p + c2p
    case DChoice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
        case (End,End) => Tau + Tau
        case (c1p,End) => c1p + Tau
        case (End,c2p) => Tau + c2p
        case (c1p,c2p) => c1p + c2p    
    case Loop(c2) => Loop(projAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case _:In | _:Out => End

  type MActions = Multiset[Action]

//  def next(l:Local): Set[(Action,Local)] =
//    l.next
//
//  def accept(l:Local): Boolean =
//    l.accepting

  def next[S](sos:SOS[Action,S],proj:Set[S], netw:MActions): Set[(Action,Set[S],MActions)] =
//  def next(proj:Set[Choreo], netw:MActions): Set[(Action,Set[Choreo],MActions)] =
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
        
        
        
        


