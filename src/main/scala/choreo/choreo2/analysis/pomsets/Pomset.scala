package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.syntax.{Agent, Msg}

/**
 * Nested pomsets
 * @param events
 * @param labels
 * @param order
 */
case class Pomset(events: Set[Event], labels: Labels, order:Set[Order]):

  lazy val agents:Set[Agent] =
    labels.values.flatMap(l=> l.agents).toSet

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.actives.contains(a)).keySet
    
  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.actives.contains(a))
    
  def sequence(other:Pomset):Pomset =
    val p = this.freshEvents(other)
    val seq = for a <- p.agents
                  in <- p.eventsOf(a)
                  inOther <- other.eventsOf(a)
      yield Order(in,inOther)
    Pomset(p.events++other.events,p.labels++other.labels,p.order++other.order++seq)

  def >>(other:Pomset):Pomset = this.sequence(other)

  /**
   * Product (parallel) of two pomsets (interleaving)
   * @param other
   * @return
   */
  def product(other:Pomset):Pomset =
    val p = this.freshEvents(other)
    Pomset(p.events++other.events,p.labels++other.labels,p.order++other.order)

  def *(other:Pomset):Pomset = this.product(other)
  
  def choice(other:Pomset):Pomset =
    val p = this.freshEvents(other)
    val e = (p.events ++ other.events).max+1
    Pomset(Set(e),Map(e->Poms(Set(p,other))),Set(Order(e,e)))
  
  def +(other:Pomset):Pomset = this.choice(other)
  
  /**
   * Given two pomsets, generates fresh event ids for this pomset,
   * replacing accordingly, events, labels and orders
   * in order to avoid overlaping with events from the other pomset.
   * @param other pomset with whom to avoid overlap
   * @return same pomset with fresh event id's
   */
  private def freshEvents(other:Pomset):Pomset =
//    if events.intersect(other.events).isEmpty then this 
//    else 
    val maxEvent = (this.events ++ other.events).max
    val fresh:Map[Event,Event] = this.events.zip(LazyList from (maxEvent+1)).toMap
    Pomset(fresh.values.toSet,
      labels.map({case(e,l)=>(fresh(e),l)}),
      order.map(o =>Order(fresh(o.left),fresh(o.right))))

object Pomset:
  type Event = Int
  type Labels = Map[Event,Label]

  val identity:Pomset = Pomset(Set(),Map(),Set())

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
  case class Poms(pomsets: Set[Pomset]) extends Label

  case class Order(left:Event,right:Event)
