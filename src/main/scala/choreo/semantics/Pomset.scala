package choreo.semantics

import choreo.Agent
import choreo.semantics.Pomset.{Event, Label, Labels, Loops, Order}
import choreo.semantics.Pomset.Label.{In, Out, OverrideIn, Role}

/**
 * Created by guillecledou on 31/10/2020
 */


case class Pomset(events: Set[Event], labels: Labels, order:Set[Order],loops:Loops):

  /**
   * Product of two pomsets (interleaving)
   * @param other
   * @return
   */
  def product(other:Pomset):Pomset =
    val p = if events.intersect(other.events).isEmpty then this else this.freshEvents(other)
    Pomset(p.events++other.events,p.labels++other.labels,p.order++other.order,p.loops++other.loops)

  def *(other:Pomset):Pomset = this.product(other)

  def sync(other:Pomset):Pomset = (this*other).sync

  def sync:Pomset =
    val syncs = for a <- this.agents; in <- allInEventsOf(a); out <- outEventsOf(a)
        yield Order(in,out)
    Pomset(events,labels,order++syncs,loops)

  def sequence(other:Pomset):Pomset =
    val p = if events.intersect(other.events).isEmpty then this
      else this.freshEvents(other)
    val seq = for a <- p.agents; in <- p.eventsOf(a) ++ p.receiversOf(a); inOther <- other.eventsOf(a)
        yield Order(in,inOther)
    Pomset(p.events++other.events,p.labels++other.labels,p.order++other.order++seq,p.loops++other.loops)

  def >>(other:Pomset):Pomset = this.sequence(other)

  lazy val agents:Set[Agent] =
    labels.values.map(_.active).toSet

  lazy val inLabels:Set[Label] =
    labels.values.filter(l=>l.role==In).toSet

  lazy val outLabels:Set[Label] =
    labels.values.filter(l=>l.role==Out).toSet

  lazy val overrideInLabels:Set[Label] =
    labels.values.filter(l=>l.role==OverrideIn).toSet

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.active==a).keySet

  def receiversOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.passive.contains(a) && (l._2.role == In || l._2.role == OverrideIn)).keySet

  def inEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==In)

  def allInEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==In || labels(e).role==OverrideIn)

  def outEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==Out)

  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.active==a)

  def activeOf(e:Event):Agent =
    labels(e).active

  def orderOf(a:Agent):Set[Order] =
    order.filter(o=> activeOf(o.left) == a && activeOf(o.right) == a)

  def loopsOf(a: Agent):Loops =
    loops.map(l => l.intersect(eventsOf(a)))

  def project(a:Agent):Pomset =
    Pomset(eventsOf(a),labelsOf(a),orderOf(a),loopsOf(a))

  /**
    * Given two pomsets, generates fresh event ids for this pomset,
    * replacing accordingly, events, labels, and loops,
    * in order to avoid overlaping with events from the other pomset.
    * @param other
    * @return
    */
  private def freshEvents(other:Pomset):Pomset =
    val maxEvent = (this.events ++ other.events).max
    val fresh:Map[Event,Event] = this.events.zip(LazyList from (maxEvent+1)).toMap
    Pomset(fresh.values.toSet,
      labels.map({case(e,l)=>(fresh(e),l)}),
      order.map({case Order(e1,e2)=>Order(fresh(e1),fresh(e2))}),
      loops.map(_.map(e=>fresh(e))))

object Pomset {
  type Event = Int
  type Labels = Map[Event,Label]
  type Loops = Set[Set[Event]]

  val identity:Pomset = Pomset(Set(),Map(),Set(),Set())

  case class Label(active:Agent,passive:Set[Agent],role:Role):
    def matchingIO(other:Label):Boolean = (this,other) match
      case (Label(a,to,Out), Label(b,from,In)) => from.contains(a)
      case (Label(a,to,Out), Label(b,from,OverrideIn)) => from.contains(a)
      case _ => false

  object Label:

    sealed trait Role
    object In extends Role
    object Out extends Role
    object OverrideIn extends Role

  case class Order(left:Event,right:Event)
}