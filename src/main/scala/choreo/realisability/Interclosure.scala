package choreo.realisability

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Event, Order,add,invert}
import choreo.syntax.Agent
import choreo.syntax.Choreo.Action
import choreo.realisability._
import choreo.realisability.Topology._

/**
 * Created by guillecledou on 01/07/2021
 */

object Interclosure:

  def apply(p:NPomset):Order =
    apply(p.projectMap)

  def apply(poms: Map[Agent, NPomset]): Order =
    val agents = poms.keySet
    var ic: Order = Map()
    for a <- agents; b <- agents; if a != b do
      ic = add(interclosure(a, poms(a), b, poms(b)), ic)
    ic

  protected def interclosure(a: Agent, pa: NPomset, b: Agent, pb: NPomset): Order =
    var ic: Order = Map()
    for as <- pa.actions.values
        bs <- pb.actions.values
    do if as.matchingOI(bs) then ic = add(interclosure(a, as, pa, b, bs, pb), ic)
    ic

  protected def interclosure(a: Agent, as: Action, pa: NPomset,
                             b: Agent, bs: Action, pb: NPomset): Order =
    val la = mkTopology(as, pa.project(as))
    val lb = mkTopology(bs, pb.project(bs))
    //println(s"[interclosure] - topology of $as :\n $la")
    //println(s"[interclosure] - topology of $bs :\n $lb")
    linkLevels(la.levels, lb.levels)

  protected def linkLevels(la: Option[Level[Event]], lb: Option[Level[Event]]): Order = (la, lb) match
    case (Some(l1), Some(l2)) => linkLevels(l1, l2)
    case _ => Map() // todo: throw error if one has one more level than the other.

  protected def linkLevels(la: Level[Event], lb: Level[Event]): Order =
    var ic: Order = Map()
    for a <- la.elems; b <- lb.elems do
      ic = add((b, a), ic)
    add(linkLevels(la.next, lb.next), ic)

  protected def mkTopology(act: Action, p: NPomset): Topology[Event] =
    val events = p.events.toSet.filter(e=>p.actions(e) == act)
    //extend succ and preds
    val pred = p.pred ++ (events--p.pred.keySet).map(e=>e->Set())
    val succ = p.succ ++ (events--p.succ.keySet).map(e=>e->Set())
    Topology(pred,succ)

