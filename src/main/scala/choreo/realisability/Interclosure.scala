package choreo.realisability

import choreo.common.MRel
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, NChoice, Nesting, Order}
import choreo.common.MRel._
import choreo.syntax.Agent
import choreo.syntax.Choreo.Action
import choreo.realisability._
import choreo.realisability.Topology._

/**
 * Created by guillecledou on 01/07/2021
 */

case class Interclosure(poms:Set[NPomset],ic:Order):
  lazy val getPom:NPomset =
    val net = getNetPom //poms.foldRight[NPomset](NPomset.empty)(_++_)
    NPomset(net.events,net.actions,(ic :++ net.pred),net.loop)
  lazy val getNetPom:NPomset =
    poms.foldRight[NPomset](NPomset.empty)(_++_)

object Interclosure:

  def apply(p:NPomset):Order =
    apply(p.projectMap)

  def apply(poms: Map[Agent, NPomset]): Order =
    val agents = poms.keySet
    var ic: Order = Map()
    val actionProj:Map[Agent,Map[Action,NPomset]] = agents.map(a =>
      a-> (for act<-poms(a).actions.values yield act->poms(a).project(act)).toMap).toMap

    for a <- agents; b <- agents; if a != b do
      ic = ic :++ interclosure(actionProj(a), actionProj(b))
    ic

  protected def interclosure(projas: Map[Action,NPomset],projbs: Map[Action,NPomset]): Order =
    var ic: Order = Map()
    for acta <- projas.keySet
        actb <- projbs.keySet
    do if acta.matchingOI(actb) then ic = ic :++ interclosure(acta, projas(acta), actb,projbs(actb))
    ic

  protected def interclosure(acta: Action, pa: NPomset,
                             actb: Action, pb: NPomset): Order =
    val la = mkTopology(acta, pa)
    val lb = mkTopology(actb, pb)
    //println(s"[interclosure] - topology of $as :\n $la")
    //println(s"[interclosure] - topology of $bs :\n $lb")
    linkLevels(la.levels, lb.levels)

  protected def linkLevels(la: Option[Level[Event]], lb: Option[Level[Event]]): Order = (la, lb) match
    case (Some(l1), Some(l2)) => linkLevels(l1, l2)
    case _ => Map() // todo: throw error if one has one more level than the other.

  protected def linkLevels(la: Level[Event], lb: Level[Event]): Order =
    var ic: Order = Map()
    for a <- la.elems; b <- lb.elems do
      ic = ic :+ (b, a)
    ic :++ linkLevels(la.next, lb.next)

  protected def mkTopology(act: Action, p: NPomset): Topology[Event] =
    val events = p.events.toSet.filter(e=>p.actions(e) == act)
    //println(s"[mktop] - act: $act")
    //println(s"[mktop] - events: $events")
    //extend succ and preds
    //println(s"[mktop] - p.pred: ${p.pred}")
    //println(s"[mktop] - p.succ: ${p.succ}")
    val pred = p.pred ++ (events--p.pred.keySet).map(e=>e->Set())
    val succ = p.succ ++ (events--p.succ.keySet).map(e=>e->Set())
    //println(s"[mktop] - pred: $pred")
    //println(s"[mktop] - succ: $succ")
    Topology(pred,succ)

