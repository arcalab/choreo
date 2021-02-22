package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.GlobalPom.min
import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.syntax.Choreo.{Action, In, Out}
import choreo.choreo2.syntax.{Agent, Msg}
import choreo.choreo2.view.DotPomsets
import choreo.choreo2.view.DotPomsets.dotPomset

/**
 * Nested pomsets
 * @param events
 * @param labels
 * @param order
 */
case class Pomset(events: Set[Event], labels: Labels, order:Set[Order], loop:Boolean=false):

  lazy val agents:Set[Agent] =
    labels.values.flatMap(l=> l.agents).toSet

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.actives.contains(a)).keySet
    
  lazy val subEvents:Set[Event] =
    labels.collect({case (e,p:LPoms)=>p.pomsets.flatMap(p=>p.events)}).flatten.toSet

  lazy val uniqueEvents:Set[Event] = events -- subEvents

  lazy val subOrders:Set[Order] =
    labels.collect({case (e,p:LPoms)=>p.pomsets.flatMap(p=>p.order)}).flatten.toSet

  lazy val uniqueOrders:Set[Order] = order -- subOrders
  
  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.actives.contains(a))

  def ordersOf(a:Agent):Set[Order] =
    order.filter(o => labels(o.left).actives.contains(a) 
      && labels(o.right).actives.contains(a))
  
  //def project(a:Agent):Pomset = 
  //  val tc = this.transitiveClosure
  //  val la = tc.labelsOf(a).map(l => (l._1, l._2 match {
  //      case LPoms(ps) => LPoms(ps.map(p=>p.project(a)))
  //      case lbl => lbl  
  //    }))
  //  Pomset(tc.eventsOf(a),la,tc.ordersOf(a),loop)

  def project(a:Agent):Pomset =
    val tc = this.transitiveClosure
    val la = tc.labels.map(l => (l._1, l._2 match {
      case LPoms(ps) => LPoms(ps.map(p=>p.project(a)))
      case lbl => if lbl.actives.contains(a) then lbl else LPoms(Set(identity))
    }))
    Pomset(tc.events,la,tc.order,loop)

  def sequence(other:Pomset):Pomset =
    val o = other.encapsulate
    val t = this.encapsulate.freshEvents(o)
    //val p = this.freshEvents(other)
    val seq = for a <- t.agents
                  in <- t.eventsOf(a)
                  inOther <- o.eventsOf(a)
      yield Order(in,inOther)
    Pomset(t.events++o.events,t.labels++o.labels,t.order++o.order++seq)

  def >>(other:Pomset):Pomset = this.sequence(other)

  /**
   * Product (parallel) of two pomsets (interleaving)
   * @param other
   * @return
   */
  def product(other:Pomset):Pomset =
    val o = other.encapsulate
    val t = this.encapsulate.freshEvents(o)
    Pomset(t.events++o.events,t.labels++o.labels,t.order++o.order)

  def *(other:Pomset):Pomset = this.product(other)

  def choice(other:Pomset):Pomset =
    val p = this.freshEvents(other)
    val e = if (p.events ++ other.events).nonEmpty then (p.events ++ other.events).max+1 else 0
    //Pomset(Set(e),Map(e->Poms(Set(p,other))),Set(Order(e,e)))
    Pomset(p.events++other.events++Set(e),
      p.labels++other.labels++Map(e->LPoms(Set(p,other))),
      p.order++other.order++(p.events++other.events).map(e1=>Order(e,e1))+Order(e,e)) // for arrows between subponsets

  def +(other:Pomset):Pomset = this.choice(other)

  def encapsulate:Pomset =
    if (this.loop) then
      val max = if this.events.nonEmpty then this.events.max+1 else 0
      Pomset(this.events+max,
        this.labels++Map(max->LPoms(Set(this))),
        this.order++this.events.map(e=>Order(max,e))+Order(max,max))
    else this

  /**
   * Given two pomsets, generates fresh event ids for this pomset,
   * replacing accordingly, events, labels and orders
   * in order to avoid overlaping with events from the other pomset.
   * @param other pomset with whom to avoid overlap
   * @return same pomset with fresh event id's
   */
  def freshEvents(other:Pomset):Pomset =
    if events.intersect(other.events).isEmpty then this
    //if allEvents.intersect(other.allEvents).isEmpty then this
    else
      //val max = (this.allEvents ++ other.allEvents).max
      val max = (this.events ++ other.events).max
      //val fresh:Map[Event,Event] = this.allEvents.zip(LazyList from (max+1)).toMap
      val fresh:Map[Event,Event] = this.events.zip(LazyList from (max+1)).toMap
      this.renameEvents(fresh)

  protected def renameEvents(rename:Map[Event,Event]):Pomset =
    val ne = events.map(e=>rename(e))
    val nl = labels.map({case(e,l)=>(rename(e), renamelbl(rename,l))})
    val no = order.map(o =>Order(rename(o.left),rename(o.right)))
    Pomset(ne,nl,no,loop)

  protected def renamelbl(rename:Map[Event,Event],l:Label):Label = l match {
    case LPoms(ps) => LPoms(ps.map(p => p.renameEvents(rename)))
    case _ => l
  }

  /**
   * Transitive reduction of a pomset
   * @return same pomset with transitively reduced dependencies
   */
  def reduce:Pomset =
    val tcPom = this.transitiveClosure
    val nonReflex = tcPom.order.filterNot(o=>o.left==o.right)
    var reduced: Map[Event, Set[Event]] = nonReflex.groupBy(o=> o.left).map(e=>e._1->e._2.map(_.right))
    
    def reachable(e:Event):Set[Event] = 
      if reduced.isDefinedAt(e) then reduced(e)++reduced(e).flatMap(e1=>reachable(e1)) else Set()

    for (e1<-events;e2<-events; if reduced.contains(e1) && reduced(e1).contains(e2))  // && e1!=e2)
      reduced = reduced.updated(e1,reduced(e1)--reachable(e2))

    val norder = reduced.flatMap(l=> l._2.map(r=>Order(l._1,r))).toSet
    val nlabels = tcPom.labels.map(l => l._2 match {
      case LPoms(ps) => (l._1,LPoms(ps.map(_.reduce)))
      case _ => l})

    Pomset(events,nlabels,norder,loop)
  

  /**
    * Naive transitive closure
    * @return
   */
  def transitiveClosure: Pomset =
    val edges: Map[Event, Set[Event]] = order.groupBy(o=> o.left).map(e=>e._1->e._2.map(_.right))
    var tc: Map[Event,Set[Event]] = Map()

    for (e<-events)
      tc = visit(e,e,edges,tc)

    val norder = tc.flatMap(t=> t._2.map(e2=>Order(t._1,e2))).toSet
    val nlabels = labels.map(l => l._2 match {
      case LPoms(ps) => (l._1,LPoms(ps.map(_.transitiveClosure)))
      case _ => l
    })
    Pomset(events,nlabels,norder,loop)

  def visit(from:Event, to:Event,
                      edges:Map[Event,Set[Event]],
                      closure:Map[Event,Set[Event]]): Map[Event,Set[Event]] =
    var tc = closure.updatedWith(from)(nodes => Some(nodes.getOrElse(Set()) + to))
    if (edges.isDefinedAt(to)) then
      for (e <- edges(to))
        if !tc(from).contains(e) then
          tc = visit(from, e, edges,tc)
    tc


object Pomset:
  type Event = Int
  type Labels = Map[Event,Label]

  val identity:Pomset = Pomset(Set(),Map(),Set())

  sealed trait Label:
    def agents:Set[Agent] = this match
      //case LIn(a, b,_) => Set(a, b) // todo: check if should be only a,same below for b
      //case LOut(b, a,_) => Set(b, a)
      case LAct(In(a,b,_)) => Set(a,b)
      case LAct(Out(a,b,_)) => Set(a,b)
      case LPoms(ps) => ps.flatMap(p=>p.agents)
      case _ => Set() // tau to avoid warnings

    def actives:Set[Agent] = this match
      //case LIn(a, _, _) => Set(a)
      //case LOut(b, _, _) => Set(b)
      case LAct(In(b,_,_)) => Set(b)
      case LAct(Out(a,_,_)) => Set(a)
      case LPoms(ps) => ps.flatMap(p => p.labels.values.flatMap(l => l.actives).toSet)
      case _ => Set() // tau to avoid warnings

    def matchingIO(other:Label):Boolean = (this,other) match
      //case (LOut(a, to, m1), LIn(b, from, m2)) => from == a && m1 == m2
      case (LAct(Out(a, to, m1)),LAct(In(b,from,m2))) => from == a && m1 == m2
      case _ => false

    def isFinal:Boolean = this match 
      case LPoms(pomsets) => pomsets.forall(p=> p == identity || p.labels.values.forall(_.isFinal))
      case LAct(act) => false
    

    def simple:Boolean = this match 
      //case LIn(_, _, _) | LOut(_, _, _) => true
      case LAct(_) => true
      case _ => false
    end simple

  //case class LIn(active:Agent,passive:Agent,msg:Msg) extends Label
  //case class LOut(active:Agent, passive:Agent,msg:Msg) extends Label
  case class LPoms(pomsets: Set[Pomset]) extends Label
  case class LAct(act:Action) extends Label

  case class Order(left:Event,right:Event)