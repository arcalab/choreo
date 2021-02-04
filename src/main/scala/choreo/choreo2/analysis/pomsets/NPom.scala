package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.NPom.{Event, LPomset, Label, Labels, Order, Poms, SPomset}
import choreo.choreo2.syntax.{Agent, Msg}

/**
 * Created by guillecledou on 19/01/2021
 */
@deprecated
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

//  lazy val subEvents:Set[Event] =
//    labels.collect({case (e,p:Poms)=>p.pomsets.flatMap(p=>p.events)}).flatten.toSet
//
//  lazy val uniqueEvents:Set[Event] = events -- subEvents

  lazy val allEvents:Set[Event] =
    events ++ labels.collect({case (e,Poms(ps)) => ps.flatMap(p=>p.allEvents)}).flatten
  
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
      t.singleton >> o
    case (t:SPomset,o:LPomset) => //t >> (o + NPom.identity)
      t >> o.singleton
    case (t:LPomset,o:LPomset) => //(t + NPom.identity) >> (o + NPom.identity)
      t.singleton >> o.singleton
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
      t.singleton * o
    case (t:SPomset,o:LPomset) => //t * (o + NPom.identity)
      t * o.singleton 
    case (t:LPomset,o:LPomset) => //(t + NPom.identity) * (o + NPom.identity)
      t.singleton * o.singleton

  def *(other:NPom):NPom = this.product(other)

  def choice(other:NPom):NPom = 
      val p = this.freshEvents(other)
      val e = (p.allEvents ++ other.allEvents).max+1
      SPomset(Set(e),Map(e->Poms(Set(p,other))),Set(Order(e,e)))
      // to add arrows between subpomsets
//      SPomset(p.events++other.events++Set(e),p.labels++other.labels++Map(e->Poms(Set(p,other))),Set(Order(e,e))) 
  
  def +(other:NPom):NPom = this.choice(other)

  def loopOnce:NPom = this match
    case p@LPomset(e,l,o) => SPomset(e,l,o) + p
    case p => p 
  
  def toSPomset:SPomset = this match 
    case LPomset(e, l, o) => SPomset(e,l,o) 
    case p:SPomset => p

  // encapsulate a loop pomset into node with the loop as only label (single choice)
  def singleton:SPomset = this match 
    case LPomset(e,l,o) => 
      val max = allEvents.max +1
      SPomset(Set(max),Map(max->Poms(Set(this))),Set(Order(max,max)))
      // to add arrows between subpomsets
      // SPomset(p.events++Set(max),p.labels++Map(max->Poms(Set(p))),Set(Order(max,max)))
    case p:SPomset => p 
  
  /**
   * Given two pomsets, generates fresh event ids for this pomset,
   * replacing accordingly, events, labels and orders
   * in order to avoid overlaping with events from the other pomset.
   * @param other pomset with whom to avoid overlap
   * @return same pomset with fresh event id's
   */
  protected def freshEvents(other:NPom):NPom = 
    if allEvents.intersect(other.allEvents).isEmpty then this
    else
      val max = (this.events ++ other.events).max
      val fresh:Map[Event,Event] = this.allEvents.zip(LazyList from (max+1)).toMap
      this.renameEvents(fresh)

  protected def renameEvents(rename:Map[Event,Event]):NPom =
    val ne = events.map(e=>rename(e))
    val nl = labels.map({case(e,l)=>(rename(e), renamelbl(rename,l))})
    val no = order.map(o =>Order(rename(o.left),rename(o.right)))
    this match
      case p:SPomset => SPomset(ne,nl,no)
      case _ => LPomset(ne,nl,no)

  protected def renamelbl(rename:Map[Event,Event],l:Label):Label = l match {
    case Poms(ps) => Poms(ps.map(p => p.renameEvents(rename)))
    case _ => l 
  }

  /**
   * Transitive reduction of a pomset
   * @return same pomset with transitively reduced dependencies
   */
  def reduce:NPom = {
    val tcPom = this.transitiveClosure
    val nonReflex = tcPom.order.filterNot(o=>o.left==o.right)
    var reduced: Map[Event, Set[Event]] = nonReflex.groupBy(o=> o.left).map(e=>e._1->e._2.map(_.right))

    for (e1<-events;e2<-events; if reduced.contains(e1) && reduced(e1).contains(e2)) // && e1!=e2)
      for(e3<-events; if reduced.contains(e2) && reduced(e2).contains(e3)) // && e2!=e3)
        reduced = reduced.updated(e1,reduced(e1)-e3)

    val norder = reduced.flatMap(l=> l._2.map(r=>Order(l._1,r))).toSet
    val nlabels = tcPom.labels.map(l => l._2 match {
      case Poms(ps) => (l._1,Poms(ps.map(_.reduce)))
      case _ => l})

    this match {
      case s:SPomset => SPomset(events,nlabels,norder)
      case _=> LPomset(events,nlabels,norder)
    }
  }

  /**
   * Naive transitive closure
   * @return
   */
  protected def transitiveClosure: NPom = {
    val edges: Map[Event, Set[Event]] = order.groupBy(o=> o.left).map(e=>e._1->e._2.map(_.right))
    var tc: Map[Event,Set[Event]] = Map()

    for (e<-events)
      tc = visit(e,e,edges,tc)

    val norder = tc.flatMap(t=> t._2.map(e2=>Order(t._1,e2))).toSet
    val nlabels = labels.map(l => l._2 match {
      case Poms(ps) => (l._1,Poms(ps.map(_.transitiveClosure)))
      case _ => l})
    
    this match {
      case s:SPomset => SPomset(events,nlabels,norder)
      case _=> LPomset(events,nlabels,norder)  
    }
    
  }

  protected def visit(from:Event, to:Event,
                      edges:Map[Event,Set[Event]],
                      closure:Map[Event,Set[Event]]): Map[Event,Set[Event]] = {
    var tc = closure.updatedWith(from)(nodes => Some(nodes.getOrElse(Set()) + to))
    if (edges.isDefinedAt(to)) then
    for (e <- edges(to))
      if !tc(from).contains(e) then
        tc = visit(from, e, edges,tc)
    tc
  }

}

@deprecated
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

    def simple:Boolean = this match {
      case LIn(_, _, _) | LOut(_, _, _) => true
      case _ => false
    }
  
  case class LIn(active:Agent,passive:Agent,msg:Msg) extends Label
  case class LOut(active:Agent, passive:Agent,msg:Msg) extends Label
  case class Poms(pomsets: Set[NPom]) extends Label

  case class Order(left:Event,right:Event)

}