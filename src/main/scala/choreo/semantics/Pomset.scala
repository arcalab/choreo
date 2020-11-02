package choreo.semantics

import choreo.Agent
import choreo.semantics.Pomset.{Event, Label, Labels, Order}
import choreo.semantics.Pomset.Label.{In, Out, OverrideIn, Role}

/**
 * Created by guillecledou on 31/10/2020
 */


case class Pomset(events: Set[Event], labels: Labels, order:Set[Order]) {

  /**
   * Product of two pomsets (interleaving)
   * Assumes they don't share events
   * todo: maybe generalize for pomsets with matching events
   * @param other
   * @return
   */
  def product(other:Pomset):Pomset =
    Pomset(events++other.events,labels++other.labels,order++other.order)

  def *(other:Pomset):Pomset = this.product(other)

  def sync(other:Pomset):Pomset = (this*other).sync

  def sync:Pomset = {
    val newOrder = for (a <- this.agents; in <- allInEventsOf(a); out <- outEventsOf(a))
        yield Order(in,out)
    Pomset(events,labels,order++newOrder)
  }

  def sequence(other:Pomset):Pomset = {
    val newOrder =
      for (a <- this.agents; in <- eventsOf(a); inOther <- other.eventsOf(a))
        yield Order(in,inOther)
    Pomset(events++other.events,labels++other.labels,order++other.order++newOrder)
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

//  def labelsOf(a:Agent):Set[Label] = {
//    labels.values.filter(l=>l.active == a).toSet
//  }

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.active==a).keySet

  def inEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==In)

  def allInEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==In || labels(e).role==OverrideIn)

  def outEventsOf(a:Agent):Set[Event] =
    eventsOf(a).filter(e=> labels(e).role==Out)

  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.active==a)
}

object Pomset {
  type Event = Int
  type Labels = Map[Event,Label]

  val identity:Pomset = Pomset(Set(),Map(),Set())

  case class Label(active:Agent,passive:Set[Agent],role:Role)

  object Label {

    sealed trait Role
    object In extends Role
    object Out extends Role
    object OverrideIn extends Role
  }

  case class Order(left:Event,right:Event)
}