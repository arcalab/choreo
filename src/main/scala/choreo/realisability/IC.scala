package choreo.realisability

/**
 * Created by guillecledou on 12/07/2021
 */
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, Order, add, invert, toPair}
import choreo.npomsets.NPomDAG._
import choreo.syntax.Agent
import choreo.syntax.Choreo.Action
import choreo.realisability._
import choreo.realisability.Topology._
import choreo.Utils
import choreo.realisability.CCPOM._


/**
 * Created by guillecledou on 07/07/2021
 *
 * Roberto Guanciale, Emilio Tuosto Interclosure
 * https://doi.org/10.1016/j.jlamp.2019.06.003
 */
object IC:

  def apply(p: NPomset):List[Interclosure] =
    val globalPomsets   = p.refinements
    val localBranches   = getAllLocalBranches(globalPomsets,p.agents)
    val tuples          = getTuples(localBranches)
    (for t<-tuples ; ics<- apply(t.toMap)(using true) yield ics).flatten.toList

  /* poms is network of projected pomsets, one for each agent */
  def apply(poms: Map[Agent, NPomset])(using complete:Boolean = true): Option[List[Interclosure]] =
    val actions = poms.map(_._2.actions).foldRight[Actions](Map())(_++_)
    if wellFormed(actions) then Some(interclosure(poms))
    else None
  
  protected def interclosure(poms: Map[Agent, NPomset]): List[Interclosure] =
    val agents = poms.keySet
    val actionProj:Map[Agent,Map[Action,NPomset]] = agents.map(a =>
      a-> (for act<-poms(a).actions.values yield act->poms(a).project(act)).toMap).toMap

    val ic: Set[Set[Order]] =
      for a <- agents
          b <- agents
          if a != b
      yield interclosure(actionProj(a), actionProj(b))

    crossOrder(ic).toList match
      case Nil  => Interclosure(poms.values.toSet,Map())::Nil
      case l    => l.map(o=>Interclosure(poms.values.toSet,o))

  //cc2 checks complete (in==out) for all actions, cc3 doesn't, for all ins (in<=out)
  protected def wellFormed(actions:Actions)(using complete:Boolean): Boolean =
    val act2e = actions.groupMap(_._2)(_._1)
    val acts = act2e.keySet
    if complete then
      acts.forall(i=>act2e.getOrElse(i,Set()).size == act2e.getOrElse(i.dual,Set()).size) // todo: out>=in>=1 if [+]?
    else
      acts.filter(_.isIn).forall(i=>act2e.getOrElse(i,Set()).size<=act2e.getOrElse(i.dual,Set()).size)

  protected def interclosure(projas: Map[Action,NPomset], projbs: Map[Action,NPomset]): Set[Order] =
    val ic: Iterable[Set[Order]] =
      for acta <- projas.keySet
          actb <- projbs.keySet
          if acta.matchingOI(actb)
      yield interclosure(acta, projas(acta), actb,projbs(actb))
    crossOrder(ic.toSet)

  protected def interclosure(acta: Action, pa: NPomset,
                             actb: Action, pb: NPomset): Set[Order] =
    val la = mkTopology(acta, pa)
    val lb = mkTopology(actb, pb)
    linkLevels(la.levels, lb.levels)

  protected def linkLevels(la: Option[Level[Event]], lb: Option[Level[Event]]): Set[Order] = (la, lb) match
    case (Some(l1), Some(l2)) => linkLevels(l1, l2)
    case _ => Set()

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

  protected def crossOrder(set:Set[Set[Order]]):Set[Order] =
    val setMaps = Utils.crossProduct(set.map(_.toList).toList.filter(l=>l.nonEmpty))
    (for s<-setMaps yield s.foldRight[Order](Map())({case (a,n) => add(n,a)})).toSet