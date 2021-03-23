package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.Pomset.Order
import choreo.choreo2.analysis.pomsets.NPomset._
import choreo.choreo2.syntax.{Agent, Choreo, Msg}
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.given_LTS_Choreo

/**
 * Created by guillecledou on 23/03/2021
 */

object ChoreoNPomset:

  private var seed:Int = 0
  private def next():Int = {seed+=1; seed-1}

  def apply(c:Choreo):NPomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p = ps.foldRight(identity)(_>>_)
      updSeedAndReturn(p)
    case Seq(c1, c2) => updSeedAndReturn(apply(c1) >> apply(c2))
    case Par(c1, c2) => updSeedAndReturn(apply(c1) * apply(c2))
    case Choice(c1, c2) => updSeedAndReturn(apply(c1) + apply(c2))
    case d@DChoice(c1, c2) =>
      dchoice2PomViaChor(d)
    //dchoice2PomViaPom(d)
    case Loop(c) =>
      val pc =  apply(c)
      val l = identity + (pc >> NPomset(pc.events,pc.labels,pc.order,pc.nested,true))
      updSeedAndReturn(l)
    case act@In(b,a,m):Action => action(act)
    case act@Out(a,b,m):Action => action(act)
    case End => identity
    case Tau:Action => identity //todo: check
  
  protected def action(act:Action):NPomset =
    val e = next()
    NPomset(Set(e),Map(e->SLabel(act)),Set(Order(e,e)),Set())
  
  protected def send(from:Agent,to:Agent,m:Msg):NPomset =
    val e1 = next()
    val e2 = next()
    NPomset(Set(e1,e2) 
      , Map(e1->SLabel(Out(from,to,m)),e2->SLabel(In(to,from,m)))
      , Set(Order(e1,e2)) 
      , Set())

  protected def updSeedAndReturn(p:NPomset):NPomset =
    if p.events.nonEmpty then
      seed = p.events.max.max(seed)+1
    p

  private def dchoice2PomViaChor(d:DChoice):NPomset =
    val next:Set[(Action,Choreo)] = d.trans
    val nextPoms:Set[NPomset] = next.map(n=>dchoice(n._1,n._2))
    var p:NPomset = identity
    if (nextPoms.size == 1)
      then p = nextPoms.head
      else p = updSeedAndReturn(bigChoice(nextPoms))
    if d.accepting then p = p + identity
      updSeedAndReturn(p)

  private def dchoice(by:Action,to:Choreo):NPomset =
    val p =  apply(to)
    updSeedAndReturn(SLabel(by)->p)