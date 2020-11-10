package choreo.semantics

import choreo.Agent
import choreo.semantics.Pomset.{Event, Label, Labels, Loops, Order}
import choreo.semantics.Pomset.Label.{In, Out, OverrideIn, Role}

/**
 * Created by guillecledou on 31/10/2020
 */


case class Pomset(events: Set[Event], labels: Labels, order:Set[Order],loops:Loops) {

  /**
   * Product of two pomsets (interleaving)
   * Assumes they don't share events
   * todo: adapt for possibly sharing events
   * @param other
   * @return
   */
  def product(other:Pomset):Pomset =
    Pomset(events++other.events,labels++other.labels,order++other.order,loops++other.loops)

  def *(other:Pomset):Pomset = this.product(other)

  def sync(other:Pomset):Pomset = (this*other).sync

  def sync:Pomset = {
    val newOrder = for (a <- this.agents; in <- allInEventsOf(a); out <- outEventsOf(a))
        yield Order(in,out)
    Pomset(events,labels,order++newOrder,loops)
  }

  def sequence(other:Pomset):Pomset = {
    val newOrder =
      for (a <- this.agents; in <- eventsOf(a) ++ receiversOf(a); inOther <- other.eventsOf(a))
        yield Order(in,inOther)
    Pomset(events++other.events,labels++other.labels,order++other.order++newOrder,loops++other.loops)
  }

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
}

object Pomset {
  type Event = Int
  type Labels = Map[Event,Label]
  type Loops = Set[Set[Event]]

  val identity:Pomset = Pomset(Set(),Map(),Set(),Set())

  case class Label(active:Agent,passive:Set[Agent],role:Role) {
    def matchingIO(other:Label):Boolean = (this,other) match {
      case (Label(a,to,Out), Label(b,from,In)) => from.contains(a)
      case (Label(a,to,Out), Label(b,from,OverrideIn)) => from.contains(a)
      case _ => false
    }
  }

  object Label {

    sealed trait Role
    object In extends Role
    object Out extends Role
    object OverrideIn extends Role
  }

  case class Order(left:Event,right:Event)
}