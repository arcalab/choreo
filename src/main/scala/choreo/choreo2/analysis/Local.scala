package choreo.choreo2.analysis

import choreo.choreo2.analysis.Global
import choreo.choreo2.backend.{LTS, Multiset, Simplify}
import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.syntax.Agent

case class Local(proj:Set[Choreo], netw:Multiset[Action]):
  override def toString: String =
    s"${proj.mkString("  [X]  ")}  [${netw}]"

given LTS[Local]:
  extension(l:Local)
    def trans: Set[(Action,Local)] =
      Local.next(l.proj,l.netw).map(p=>(p._1,Local(p._2,p._3)))
    def accepting: Boolean =
      l.netw.isEmpty && l.proj.forall(c => c.taus.exists(_.accepting))

object Local:
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
  
  def next[S:LTS](proj:Set[S], netw:MActions): Set[(Action,Set[S],MActions)] =
    val x = for (p <- proj) yield  // get each projection
      val proj2 = evolveProj(p,netw) // get all evolutions = (act,newProj,newNet)
      val newProj = for (act,p2,n2)<-proj2 yield
        (act, proj-p+p2 , n2 )
      newProj
    x.flatten


  private def evolveProj[S:LTS](c:S, net:MActions): Set[(Action,S,MActions)] =
    for (act,chor)<-c.trans if allowed(act,net) yield
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
        
        
        
        
//////////////////////////////////////////////////
// Earlier attempts, with different projections //
//////////////////////////////////////////////////

case class LocalBasic(proj:Set[Choreo], netw:Multiset[Action]): // extends LTS[Local] :
  def get:LocalBasic = this
  override def toString: String =
    s"${proj.mkString("  [X]  ")}  [${netw}]"

given LTS[LocalBasic]:
  extension(l:LocalBasic)
    def trans: Set[(Action,LocalBasic)] =
      Local.next(l.proj,l.netw).map(p=>(p._1,LocalBasic(p._2,p._3)))
    def accepting: Boolean =
//      l.netw.isEmpty && l.proj.forall(c => c.accepting)
      l.proj.forall(c => c.accepting)



object LocalBasic:
  
  def apply(c:Choreo): LocalBasic =
    LocalBasic(allProj(c).map(_._2),Multiset())
  
  def nextSys(c:Choreo): Set[(Action,LocalBasic)] =
    apply(c).trans

  def nextSysPP(s:LocalBasic): Unit =
    actionsPP(s.trans)

  def nextSysPP(c:Choreo): Unit =
    actionsPP(nextSys(c))

  def actionsPP(s:Set[(Action,LocalBasic)]): Unit =
    for (a,l) <- s do
    println(s"$a: \n${
      l.proj.map(" - "+_.toString).mkString("\n") +
        "\n ---\n " +
        l.netw.toString
    }")


  /** Projects an expression into an agent.
   *  This version does not generate any tau in the projection.
   * */
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projAux(c,a))
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      //      val outs = as.filter(_==a).flatMap(a2=>bs.map(b=>a2!b by m))
      //      val ins  = bs.filter(_==a).flatMap(b=>as.map(a2=>b?a2 by m))
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projAux((outs++ins).fold(End)(_>_) , a)
    case Seq(c1, c2) => projAux(c1,a) > projAux(c2,a)
    case Par(c1, c2) => projAux(c1,a) || projAux(c2,a)
    case Choice(c1, c2) =>projAux(c1,a) + projAux(c2,a)
    case DChoice(c1, c2) =>projAux(c1,a) + projAux(c2,a)  //todo: check DChoice
    case Loop(c2) => Loop(projAux(c2,a))
    case End => End
    case Tau => Tau 
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case _:In | _:Out => End //Tau

  def allProj(c:Choreo): Set[(Agent,Choreo)] =
    (for a<-agents(c) yield a->proj(c,a))

  def allProjPP(c:Choreo): String = allProj(c).toMap.mkString("\n")



//// Local behaviour with GlobalTau
case class LocalManyTaus(proj:Set[GlobalManyTaus], netw:Multiset[Action]): // extends LTS[Local] :
  override def toString: String =
    s"${proj.mkString("  [X]  ")}  [${netw}]"
  
object LocalManyTaus:
  def apply(c:Choreo): LocalManyTaus =
    LocalManyTaus(allProjTau(c).map(p=>GlobalManyTaus(p._2)),Multiset())


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



given LTS[LocalManyTaus]:
  extension(l:LocalManyTaus)
    def trans: Set[(Action,LocalManyTaus)] =
      Local.next(l.proj,l.netw).map(p=>(p._1,LocalManyTaus(p._2,p._3)))
    def accepting: Boolean =
      l.netw.isEmpty && l.proj.forall(c => c.accepting)


