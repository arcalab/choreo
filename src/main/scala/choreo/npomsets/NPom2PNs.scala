package choreo.npomsets

import choreo.npomsets.NPomset.Event
import choreo.petrinet
import choreo.petrinet.PN
import choreo.petrinet.PN.{Place, PlaceId, Trans}
import choreo.syntax.Agent
import choreo.syntax.Choreo.{In, Out}

/**
 * Created by guillecledou on 14/02/2022
 *
 * Given and NPom it creates a set of one safe petri nets for each agent.
 * Each petri net represents an alternative behaviour of the agent.
 */
@deprecated
object NPom2PNs:

  protected var pseed:Int = 0
  protected var tseed:Int = 0
  protected def pid():Int = {pseed+=1; pseed-1}
  protected def tid():Int = {tseed+=1; tseed-1}

  def apply(npom:NPomset):Map[Agent,Set[PN]] =
    pseed = 0 ; tseed = 0
    // flatten choices
    val poms = npom.refinements
    // for each agent build its set of petri nets
    (for a <- npom.agents yield
      a -> apply(poms)(a)).toMap

  protected def apply(poms:List[NPomset])(implicit a:Agent):Set[PN] =
    for (p, i) <- poms.view.toSet.zipWithIndex ; proj = p.project(a) yield pom2pn(proj)(a,i)

  /**
   * Given a pomset (a flatten and projected pomset) builds a petri net
   * @param pom a pomset
   * @return a petri net for the pomset
   */
  protected def pom2pn(pom:NPomset)(implicit a:Agent,i:Int):PN =
    var places:Set[Place] = Set()
    var trans:Set[Trans] = Set()
    var marking:Set[PlaceId] = Set()
    // create transitions, arcs and places for each event
    for e <- pom.events.toSet do
      places += Place(e)
      val (t,init) = mkTrans(e)(pom)
      trans += t
      marking ++= (if init.isDefined then Set(init.get.id) else Set())
      places ++= init
    // create petri net
    PN(places,trans,marking,s"Prot$a$i")

  /**
   * given an envent creates a transition
   * @param e event
   * @param pom pomset
   * @return a transition for e and the set of initial places created if any
   */
  protected def mkTrans(e:Event)(implicit pom:NPomset):(Trans,Option[Place]) =
    // pre places
    var pre = pom.realPred(e)
    // make initial marking if needed
    val marking = if pre.isEmpty then Some(Place(pid())) else None
    // update pre
    pre ++= (if marking.isDefined then Set(marking.get.id) else Set())
    // post place
    val post = Set(e)
    // transition + places created
    (Trans(e, pom.actions(e).asInstanceOf[In | Out], pre, post), marking)


