package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.NPom.{Event, SPomset, LPomset, Labels, Order, Poms}
import choreo.choreo2.syntax.{Agent, Msg}

/**
 * Created by guillecledou on 19/01/2021
 */

trait NPom {
  val events: Set[Event]
  val labels: Labels
  val order:Set[Order]
    
  lazy val agents:Set[Agent] =
    labels.values.flatMap(l=> l.agents).toSet

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.actives.contains(a)).keySet

  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.actives.contains(a))

  def sequence(other:NPom):NPom = (this,other) match {
    case (t:SPomset,o:SPomset) => 
      val p = t.freshEvents(o)
      val seq = for a <- p.agents
                  in <- p.eventsOf (a)
                  inOther <- o.eventsOf (a)
                yield Order (in, inOther)
      SPomset(p.events ++ o.events, p.labels ++ o.labels, p.order ++ o.order ++ seq)
    //todo: not so sure about these:
    case (t:LPomset,o:SPomset) => //(t + NPom.identity) >> o
      singleton(t) >> o
    case (t:SPomset,o:LPomset) => //t >> (o + NPom.identity)
      t >> singleton(o)
    case (t:LPomset,o:LPomset) => //(t + NPom.identity) >> (o + NPom.identity)
      singleton(t) >> singleton(o)
  }

  def >>(other:NPom):NPom = this.sequence(other)

  /**
   * Product (parallel) of two pomsets (interleaving)
   * @param other
   * @return
   */
  def product(other:NPom):NPom = (this, other) match
    case (t:SPomset,o:SPomset) =>
      val p = t.freshEvents(o)
      SPomset(p.events++o.events, p.labels++o.labels,p.order++o.order)
    //todo: not so sure about these:
    case (t:LPomset,o:SPomset) => //(t + NPom.identity) * o
      singleton(t) + o
    case (t:SPomset,o:LPomset) => //t * (o + NPom.identity)
      t + singleton(o) 
    case (t:LPomset,o:LPomset) => //(t + NPom.identity) * (o + NPom.identity)
      singleton(t) + singleton(o)

  def *(other:NPom):NPom = this.product(other)

  def choice(other:NPom):NPom = 
      val p = this.freshEvents(other)
      val e = (p.events ++ other.events).max+1
      SPomset(Set(e),Map(e->Poms(Set(p,other))),Set(Order(e,e)))
  
  def +(other:NPom):NPom = this.choice(other)

  def once(p:LPomset):SPomset = SPomset(p.events,p.labels,p.order)
  // encapsulate a loop pomset into node with the loop as only label (single choice)
  def singleton(p:LPomset):SPomset =
    val max = p.events.max +1
    SPomset(Set(max),Map(max->Poms(Set(p))),Set(Order(max,max)))
  
  /**
   * Given two pomsets, generates fresh event ids for this pomset,
   * replacing accordingly, events, labels and orders
   * in order to avoid overlaping with events from the other pomset.
   * @param other pomset with whom to avoid overlap
   * @return same pomset with fresh event id's
   */
  def freshEvents(other:NPom):NPom = 
//    if events.intersect(other.events).isEmpty then this
//    else
    val maxEvent = (this.events ++ other.events).max
    val fresh:Map[Event,Event] = this.events.zip(LazyList from (maxEvent+1)).toMap
    val ne = fresh.values.toSet
    val nl = labels.map({case(e,l)=>(fresh(e),l)})
    val no = order.map(o =>Order(fresh(o.left),fresh(o.right)))
    this match
      case p:SPomset => SPomset(ne,nl,no)
      case _ => LPomset(ne,nl,no) 
}

object NPom{
  type Event = Int
  type Labels = Map[Event,Label]

  val identity:SPomset = SPomset(Set(),Map(),Set())

  case class SPomset(events: Set[Event], labels: Labels, order:Set[Order]) extends NPom
  case class LPomset(events: Set[Event], labels: Labels, order:Set[Order]) extends NPom

  sealed trait Label:
    def agents:Set[Agent] = this match
      case LIn(a, b,_) => Set(a, b) // todo: check if should be only a,same below for b
      case LOut(b, a,_) => Set(b, a)
      case Poms(ps) => ps.flatMap(p=>p.agents)

    def actives:Set[Agent] = this match {
      case LIn(a, _, _) => Set(a)
      case LOut(b, _, _) => Set(b)
      case Poms(ps) => ps.flatMap(p => p.labels.values.flatMap(l => l.actives).toSet)
    }

    def matchingIO(other:Label):Boolean = (this,other) match {
      case (LOut(a, to, m1), LIn(b, from, m2)) => from == a && m1 == m2
      case _ => false
    }

    def simple:Boolean = this match
      case LIn(_,_,_) | LOut(_,_,_) => true
      case _ => false

  case class LIn(active:Agent,passive:Agent,msg:Msg) extends Label
  case class LOut(active:Agent, passive:Agent,msg:Msg) extends Label
  case class Poms(pomsets: Set[NPom]) extends Label

  case class Order(left:Event,right:Event)

}