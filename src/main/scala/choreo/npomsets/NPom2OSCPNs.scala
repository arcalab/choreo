package choreo.npomsets

import choreo.npomsets.NPomset.Event
import choreo.petrinet.OneSafeColouredPN
import choreo.petrinet.OneSafeColouredPN.{DNF, Place, PlaceId, Trans, Var}
import choreo.syntax.Agent
import choreo.syntax.Choreo.{In, Out}

@deprecated
object NPom2OSCPNs:

  protected var pseed:Int = 0
  protected var tseed:Int = 0
  protected def pid():Int = {pseed+=1; pseed-1}
  protected def tid():Int = {tseed+=1; tseed-1}

  def apply(npom:NPomset):Set[OneSafeColouredPN] =
    pseed = 0 ; tseed = 0
    // flatten choices
    val poms = npom.refinements
    // for each agent build its petri net
    for a <- npom.agents yield
      apply(a,poms)

  protected def apply(a:Agent,poms:List[NPomset]):OneSafeColouredPN =
    val pns = for p <- poms ; proj = p.project(a) yield pom2pn(proj)
    val union = pns.foldRight[OneSafeColouredPN](OneSafeColouredPN())(_++_)
    merge(union)


  protected def merge(pn:OneSafeColouredPN):OneSafeColouredPN =
    // todo
    pn

  protected def merge(block:Set[PlaceId])(implicit pn:OneSafeColouredPN):(OneSafeColouredPN,Set[Set[PlaceId]]) =
    // successor transitions from block
    val succ = for t <- pn.transitions ; if t.prePlaces.intersect(block) == block yield t
    // group successors by label
    val succByLabel = succ.groupBy(_.channel)
    // merge transitions and build the guards
    val ntrans = mergeTrans(succByLabel)
    // succesors of succ
    val next = for t <- succ yield t.postPlaces
    // commit choices
    ???

  protected def mergeTrans(groups: Map[In|Out, Set[Trans]]): Set[Trans] =
    for g <- groups.view.toSet yield mergeTrans(g)

  protected def mergeTrans(group: (In | Out,Set[Trans])):Trans =
    val (ch,trs) = group
    if trs.size > 1 then
      val pre = DNF(trs.flatMap(t=> t.pre.options))
      val post = trs.flatMap(t=>t.post).toMap
      Trans(tid(),ch,pre,post)
    else trs.head

  /**
   * Given a pomset (a flatten and projected pomset) builds a petri net
   * @param pom a pomset
   * @return a petri net for the pomset
   */
  protected def pom2pn(pom:NPomset):OneSafeColouredPN =
    var places:Set[Place] = Set()
    var trans:Set[Trans] = Set()
    var marking:Set[PlaceId] = Set()
    // create transitions, arcs and places for each event
    for e <- pom.events.toSet do
      places += Place(e)
      val (t,init) = mkTrans(e)(pom)
      trans += t
      marking ++= init.map(p=>p.id)
      places ++= init
    // create petri net
    OneSafeColouredPN(places,trans,marking,"")

  /**
   * given an envent creates a transition
   * @param e event
   * @param pom pomset
   * @return a transition for e and the set of initial places created if any
   */
  protected def mkTrans(e:Event)(implicit pom:NPomset):(Trans,Set[Place]) =
    var pred = pom.realPred(e)
    var marking:Set[Place] = Set()
    // make initial marking if needed
    if pred.isEmpty then marking += Place(pid())
    // make dnf with the AND of all pre places
    val dnf = DNF(Set(pred.map(p => Var(p))))
    // make out arcs expressions
    val exp = pred.map(p=> p -> false).toMap + (e->true)
    // transition + places created
    (Trans(e, pom.actions(e).asInstanceOf[In | Out], dnf, exp), marking)
