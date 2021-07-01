package choreo.npomsets

import choreo.npomsets.NPomset.{Event, Order, add}
import choreo.syntax.Agent
import choreo.syntax.Choreo.Action

/**
 * Created by guillecledou on 01/07/2021
 */

object Interclosure:

  def apply(poms:Map[Agent,NPomset]):Order =
    val agents = poms.keySet
    var ic:Order = Map()
    for a <- agents; b <- agents; if a!=b do
      ic = add(interclosure(a,poms(a),b,poms(b)),ic)
    ic

  protected def interclosure(a:Agent,pa:NPomset,b:Agent,pb:NPomset):Order =
    var ic:Order = Map()
    //var oi:Set[(Action,Action)] = Set()
    for as <- pa.actions.values
        bs <- pb.actions.values
    do  if as.matchingOI(bs) then ic = add(interclosure(a,as,pa,b,bs,pb),ic)
    //if bs.matchingOI(as) then ic.mapset(interclosure(b,bs,pb,a,as,pa))
    ic

  protected def interclosure(a:Agent,as:Action,pa:NPomset,
                             b:Agent,bs:Action,pb:NPomset):Order =
    val la = mkTopology(as,keep(as,pa))
    val lb = mkTopology(bs,keep(bs,pb))
    linkLevels(la,lb)

  protected def mkTopology(act:Action, p:NPomset):Topology[Event] =
    var top:Topology[Event] = Topology()
    for e <- p.events.toSet
        if (!p.pred.isDefinedAt(e) || p.pred(e).isEmpty)
           //&& p.actions.isDefinedAt(e)
           && p.actions(e) == act
    do top = top.add(e)
    bfs(act,top,p).get

  protected def bfs(act:Action, l:Topology[Event], p:NPomset):Option[Topology[Event]] =
    var nl:Topology[Event] = Topology()
    for e<-l.elems; suc<-p.succ.get(e) ; ps <-suc do /*; if p.actions.isDefinedAt(ps) && p.actions(ps) == act*/
      nl = nl.add(ps)
    if nl.elems.isEmpty then Some(l)
    else Some(l.add(bfs(act,nl,p)))


  protected def linkLevels(la:Option[Topology[Event]], lb:Option[Topology[Event]]):Order = (la,lb) match
    case (Some(l1),Some(l2)) => linkLevels(l1,l2)
    case _ => Map()

  protected def linkLevels(la:Topology[Event], lb:Topology[Event]):Order =
    var ic:Order = Map()
    for a<-la.elems ; b<-lb.elems do
      ic = add((b,a),ic)
    add(linkLevels(la.next,lb.next),ic)

  protected def keep(act:Action,p:NPomset):NPomset =
    var np:Order = for (a,bs)<-p.pred
                       if p.actions.isDefinedAt(a) && p.actions(a) == act
                   yield a->bs.filter(e=>p.actions.isDefinedAt(e) && p.actions(e) == act)
    NPomset(p.events,p.actions,np,p.loop).simplified

  //val np:Order = p.pred.filter(kv=>p.actions.isDefinedAt(kv._1) && p.actions(kv._1) == action)
  //  .map({case (k,v) => (k,v.filter(e=>p.actions.isDefinedAt(e) && p.actions(e) == action))})
