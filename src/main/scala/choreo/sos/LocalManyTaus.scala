package choreo.sos

import choreo.common.{Multiset, Simplify}
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.{Action, Choice, DChoice, End, In, Loop, Out, Par, Send, Seq, Tau, agents}
import choreo.sos.{ChorDefSOS, Local, ChorManyTausSOS}

//// Local behaviour with GlobalTau
@deprecated
case class LocalManyTaus(proj:Set[Choreo], netw:Multiset[Action]): // extends LTS[Local] :
  override def toString: String =
    s"${proj.mkString("  [X]  ")}  [${netw}]"

@deprecated
object LocalManyTaus extends SOS[Action,LocalManyTaus]:
  override def accepting(l: LocalManyTaus): Boolean =
    l.netw.isEmpty && l.proj.forall(ChorDefSOS.accepting)

  override def next(l: LocalManyTaus): Set[(Action, LocalManyTaus)] =
    Local.next(ChorManyTausSOS,l.proj,l.netw).map(p=>(p._1,LocalManyTaus(p._2,p._3)))

  def apply(c:Choreo): LocalManyTaus =
    LocalManyTaus(allProjTau(c).map(p=>p._2),Multiset())


  /** Projects into an agent, using Tau to denote non-active actions. */
  def projTau(c:Choreo, a:Agent): Choreo = Simplify(projTauAux(c,a))
  private def projTauAux(c:Choreo, a:Agent): Choreo = c match
    case Send(List(`a`), List(b), m) => Out(a,b,m)
    case Send(List(b), List(`a`), m) => In(a,b,m)
    case Send(List(_), List(_), _) => Tau
    case Send(as, bs, m) =>
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projTauAux((outs++ins).fold(End)(_>_) , a)

    case Seq(c1, c2) => projTauAux(c1,a) > projTauAux(c2,a)
    case Par(c1, c2) => projTauAux(c1,a) || projTauAux(c2,a)
    case Choice(c1, c2) =>projTauAux(c1,a) + projTauAux(c2,a)
    case DChoice(c1, c2) =>projTauAux(c1,a) + projTauAux(c2,a)//todo: check DChoice
    case Loop(c2) => Loop(projTauAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c // never user
    case Out(`a`,_,_) => c // never used
    case _:In | _:Out => Tau // never used

  def allProjTau(c:Choreo): Set[(Agent,Choreo)] =
    (for a<-agents(c) yield a->projTau(c,a))


//
//given LTS[LocalManyTaus]:
//  extension(l:LocalManyTaus)
//  def next: Set[(Action,LocalManyTaus)] =
//    Local.next(l.proj,l.netw).map(p=>(p._1,LocalManyTaus(p._2,p._3)))
//  def accepting: Boolean =
//    l.netw.isEmpty && l.proj.forall(c => c.accepting)

