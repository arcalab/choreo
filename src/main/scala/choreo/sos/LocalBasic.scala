package choreo.sos

import choreo.common.{Multiset, Simplify}
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.{Action, Choice, DChoice, End, In, Loop, Out, Par, Send, Seq, Tau, agents}
import choreo.sos.{SOS,ChorBasicSOS}

//////////////////////////////////////////////////
// Earlier attempts, with different projections //
//////////////////////////////////////////////////

@deprecated
case class LocalBasic(proj:Set[Choreo], netw:Multiset[Action]): // extends LTS[Local] :
  def get:LocalBasic = this
  override def toString: String =
    s"${proj.mkString("  [X]  ")}  [${netw}]"

//given LTS[LocalBasic]:
//  extension(l:LocalBasic)
//  def next: Set[(Action,LocalBasic)] =
//    Local.next(l.proj,l.netw).map(p=>(p._1,LocalBasic(p._2,p._3)))
//  def accepting: Boolean =
//  //      l.netw.isEmpty && l.proj.forall(c => c.accepting)
//    l.proj.forall(c => c.accepting)


@deprecated
object LocalBasic extends SOS[Action,LocalBasic]:

  override def accepting(l: LocalBasic): Boolean =
    l.proj.forall(ChorBasicSOS.accepting)

  override def next(l: LocalBasic): Set[(Action, LocalBasic)] =
    Local.next(ChorBasicSOS,l.proj,l.netw).map(p=>(p._1,LocalBasic(p._2,p._3)))

  def apply(c:Choreo): LocalBasic =
    LocalBasic(allProj(c).map(_._2),Multiset())

  def nextSys(c:Choreo): Set[(Action,LocalBasic)] =
    next(apply(c))

  def nextSysPP(s:LocalBasic): Unit =
    actionsPP(next(s))

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
