package choreo.realisability

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Event, Order,add,invert}
import choreo.syntax.Agent
import choreo.syntax.Choreo.Action
import choreo.realisability._
import choreo.realisability.Topology._

/**
 * Created by guillecledou on 07/07/2021
 *
 * Emilio's interclosure
 */
object EInterclosure:

  def apply(p:NPomset):List[Order] =
    apply(p.projectMap)

  def apply(poms: Map[Agent, NPomset]): List[Order] =
    val agents = poms.keySet
    val actionProj:Map[Agent,Map[Action,NPomset]] = agents.map(a =>
      a-> (for act<-poms(a).actions.values yield act->poms(a).project(act)).toMap).toMap
    val ic: Set[Set[Order]] =
      for a <- agents; b <- agents; if a != b yield
        interclosure(actionProj(a), actionProj(b))
    crossOrder(ic).toList

  protected def interclosure(projas: Map[Action,NPomset],projbs: Map[Action,NPomset]): Set[Order] =
    val ic: Iterable[Set[Order]] =
    for acta <- projas.keySet
        actb <- projbs.keySet
        if acta.matchingOI(actb)
    yield interclosure(acta, projas(acta), actb,projbs(actb))
    crossOrder(ic.toSet)

  //protected def interclosure(a: Agent, pa: NPomset, b: Agent, pb: NPomset): Set[Order] =
  //  var ic: Iterable[Set[Order]] =
  //    for as <- pa.actions.values
  //      bs <- pb.actions.values
  //      if as.matchingOI(bs)
  //    yield interclosure(a, as, pa, b, bs, pb)
  //  crossOrder(ic.toSet)

  protected def interclosure(acta: Action, pa: NPomset,
                               actb: Action, pb: NPomset): Set[Order] =
    val la = mkTopology(acta, pa)
    val lb = mkTopology(actb, pb)
    linkLevels(la.levels, lb.levels)

  //protected def interclosure(a: Agent, as: Action, pa: NPomset,
  //                           b: Agent, bs: Action, pb: NPomset): Set[Order] =
  //  val la = mkTopology(as, pa.project(as))
  //  val lb = mkTopology(bs, pb.project(bs))
  //  linkLevels(la.levels, lb.levels)

  protected def linkLevels(la: Option[Level[Event]], lb: Option[Level[Event]]): Set[Order] = (la, lb) match
    case (Some(l1), Some(l2)) => linkLevels(l1, l2)
    case _ => Set() // todo: throw error if one has one more level than the other.

  protected def linkLevels(la: Level[Event], lb: Level[Event]): Set[Order] =
    val elemA = la.elems.toList
    var elemB = lb.elems.toList
    var ic: List[Order] = elemB.map(e=>Map())
    var k,j:Int = 0
    for a <- elemA do
      j = 0
      for r <- Range(k,k+elemB.size) do
        val i = r % elemB.size
        val b = elemB(i)
        ic = ic.updated(j,add((b, a), ic(j)))
        j+=1
      k+=1
    crossOrder(Set(linkLevels(la.next, lb.next), ic.toSet))
    //add(linkLevels(la.next, lb.next), ic)

  protected def mkTopology(act: Action, p: NPomset): Topology[Event] =
    val events = p.events.toSet.filter(e=>p.actions(e) == act)
    //extend succ and preds
    val pred = p.pred ++ (events--p.pred.keySet).map(e=>e->Set())
    val succ = p.succ ++ (events--p.succ.keySet).map(e=>e->Set())
    Topology(pred,succ)

  // cross product
  protected def crossProduct[A](set:List[List[A]]):List[List[A]] = set match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  protected def crossOrder(set:Set[Set[Order]]):Set[Order] =
    val setMaps = crossProduct(set.map(_.toList).toList.filter(l=>l.nonEmpty))
    (for s<-setMaps yield s.foldRight[Order](Map())({case (a,n) => add(n,a)})).toSet

