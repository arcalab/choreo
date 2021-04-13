package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.Pomset.{Event, Order}
import choreo.choreo2.analysis.pomsets.NPomset._
import choreo.choreo2.syntax.Agent
import choreo.choreo2.syntax.Choreo.{Action, In, Out}


case class NPomset(events: Set[Event]
                   , labels: Labels
                   , order:Set[Order] 
                   , nested:Set[Set[NPomset]]
                   , loop:Boolean=false): 

  lazy val agents:Set[Agent] =
    labels.values.flatMap(l=>l.agents).toSet

  def eventsOf(a:Agent):Set[Event] =
    labels.filter(l=>l._2.subj == a).keySet

  lazy val nestedEvents:Set[Event] =
    nested.flatMap(n=>n.flatMap(r=> r.events))

  lazy val localEvents:Set[Event] = 
    events -- nestedEvents

  lazy val nestedOrders:Set[Order] =
    nested.flatMap(n=>n.flatMap(r=>r.order))

  lazy val localOrders:Set[Order] = 
    order -- nestedOrders

  def labelsOf(a:Agent):Labels =
    labels.filter(l=>l._2.subj==a)

  def ordersOf(a:Agent):Set[Order] =
    order.filter(o => labels(o.left).subj==a && labels(o.right).subj==a)

  def nestedPom:Set[NPomset] =
    nested.flatMap(n=>n) ++ nested.flatMap(n=>n.flatMap(r=>r.nestedPom))

  //def -(e:Event):NPomset =
  //  val ne = events - e
  //  val nl = labels.removed(e)
  //  val no = order.filterNot(o=> o.left == e || o.right == e)
  //  var nn = nested.map(p=>p-e)
  //  if !nested.contains(identity) then nn = nn-identity
  //  NPomset(ne,nl,no,nn,loop)

  //def -(e:Event):NPomset =
  //  val mbC:Option[Set[NPomset]] = NPomset.localInChoices(nested,e)
  //  val mbR:Option[NPomset] = if mbC.isDefined then mbC.get.find(p=>p.events.contains(e)) else None
  //  var ne:Set[Event] = Set()
  //  var nn:Set[Set[NPomset]] = Set()
  //  if mbR.isDefined then
  //    val mbr = mbR.get.nestedPom.find(p=>p.localEvents.contains(e))
  //    nn = (nested - mbC.get) ++ mbr.get.nested
  //    ne = (localEvents ++ mbr.get.events ++ NPomset.eventsInChoices(nn)) - e
  //  else
  //    nn = NPomset.rmInChoices(nested,e)
  //    ne = (localEvents ++ NPomset.eventsInChoices(nn)) - e
  //  val nl = labels.filter(l=>ne.contains(l._1))
  //  val no = order.filter(o=>ne.contains(o.left) && ne.contains(o.right))
  //  NPomset(ne,nl,no,nn,loop)
  
  def -(e:Event):NPomset = 
     if !events.contains(e) then this 
     else 
        var ne:Set[Event] = Set()
        var nn:Set[Set[NPomset]] = Set()
        if localEvents.contains(e) then 
          ne = events -e 
          nn = nested 
        else 
          val ch = NPomset.findChoice(nested,e).get 
          val r = ch.find(p=>p.events.contains(e)).get
          val localr = if r.localEvents.contains(e) then r 
            else NPomset.findLocalInChoice(r.nestedPom,e).get
          nn = (nested - ch) ++ localr.nested
          ne = (localEvents ++ localr.events ++ NPomset.eventsInChoices(nn)) - e
        val nl = labels.filter(l=>ne.contains(l._1))
        val no = order.filter(o=>ne.contains(o.left) && ne.contains(o.right)) 
        NPomset(ne,nl,no,nn,loop)
    

  //def -(p:NPomset):NPomset =


  //  val ne = events - e
  //  val nl = labels.removed(e)
  //  val no = order.filterNot(o=> o.left == e || o.right == e)
  //  var nn = nested.map(p=>p-e)
  //  if !nested.contains(identity) then nn = nn-identity
  //  NPomset(ne,nl,no,nn,loop)

  //def --(pomsets:Set[NPomset]):NPomset =
  //  val rme = pomsets.flatMap(p=>p.events)
  //  val ne = events -- rme
  //  val nl = labels.filter(l=>ne contains l._1)
  //  val no = order.filterNot(o=> (rme contains o.left) || (rme contains o.right))
  //  val nn = (nested --pomsets).map(p=>p--pomsets)
  //  NPomset(ne,nl,no,nn,loop)

  /* Finds the pomset to which e (e in event) is local, if it exists */
  //def pomsetOf(e:Event):Option[NPomset] =
  //  if localEvents contains e then Some(this)
  //  else nested.find(p=>p.pomsetOf(e).isDefined)
  //
  ///* Finfs all pomsets that are siblings of p in a nested set*/
  //def siblingsOf(p:NPomset):Set[NPomset] =
  //  if nested.contains(p) then nested - p
  //  else nested.flatMap(np=>np.siblingsOf(p))
  
  def encapsulate:NPomset = 
    if !this.loop then this
    else NPomset(events,labels,order,Set(Set(this)))
  
  def project(a:Agent):NPomset =
    val tc = this.transitiveClosure 
    val ea = tc.eventsOf(a)
    val la = tc.labelsOf(a)
    val oa = tc.ordersOf(a)
    val na = nested.map(n=>n.map(p=>p.project(a)))
    NPomset(ea,la,oa,na,loop)

  def sequence(other:NPomset):NPomset =
    val t = this.freshEvents(other)
    val o = other.encapsulate
    val seq = for a <- t.agents
                  ea1 <- t.eventsOf(a)
                  ea2 <- o.eventsOf(a)
      yield Order(ea1,ea2)
    NPomset(t.events++o.events
      , t.labels++o.labels
      , t.order++o.order++seq
      , t.nested++o.nested)

  def >>(other:NPomset):NPomset = this.sequence(other)

  /**
   * Product (parallel) of two pomsets (interleaving)
   * @param other pomset 
   * @return this pomset in parallel with other
   */
  def product(other:NPomset):NPomset =
    val t = this.encapsulate.freshEvents(other)
    val o = other.encapsulate
    NPomset(t.events++o.events
      , t.labels++o.labels
      , t.order++o.order
      , t.nested++o.nested)

  def *(other:NPomset):NPomset = this.product(other)

  def choice(other:NPomset):NPomset =
    val t = this.freshEvents(other)
    val o = other.encapsulate
    NPomset(t.events++o.events
      , t.labels++o.labels
      , t.order++o.order 
      , //nested++o.nested+
        Set(Set(this,o)))

  def +(other:NPomset):NPomset = this.choice(other)

  /**
   * Transitive reduction of a pomset
   * @return same pomset with transitively reduced dependencies
   */
  def reduce:NPomset =
    val tc = this.transitiveClosure
    val nonReflex = tc.order.filterNot(o=>o.left==o.right)
    var reduced: Map[Event, Set[Event]] = 
      nonReflex.groupBy(o=> o.left).map(e=>e._1->e._2.map(_.right))

    def reachable(e:Event):Set[Event] =
      if reduced.isDefinedAt(e) 
        then reduced(e)++reduced(e).flatMap(e1=>reachable(e1)) 
        else Set()

    for (e1<-events;e2<-events; if reduced.contains(e1) && reduced(e1).contains(e2))  // && e1!=e2)
      reduced = reduced.updated(e1,reduced(e1)--reachable(e2))

    val no = reduced.flatMap(l=> l._2.map(r=>Order(l._1,r))).toSet
    val nn = nested.map(n=>n.map(p=>p.reduce))

    NPomset(events,labels,no,nn,loop)
  
  /**
   * Naive transitive closure
   * @return
   */
  def transitiveClosure: NPomset =
    val edges: Map[Event, Set[Event]] = order.groupBy(o=> o.left)
      .map(e=>e._1->e._2.map(_.right))
    var tc: Map[Event,Set[Event]] = Map()

    for (e<-events)
      tc = visit(e,e,edges,tc)

    val no = tc.flatMap(t=> t._2.map(e2=>Order(t._1,e2))).toSet
    val nn = nested.map(n=>n.map(p=>p.transitiveClosure))
    NPomset(events,labels,no,nn,loop)

  def visit(from:Event, to:Event,
            edges:Map[Event,Set[Event]],
            closure:Map[Event,Set[Event]]): Map[Event,Set[Event]] =
    var tc = closure.updatedWith(from)(nodes => Some(nodes.getOrElse(Set()) + to))
    if (edges.isDefinedAt(to)) then
      for (e <- edges(to))
        if !tc(from).contains(e) then
          tc = visit(from, e, edges,tc)
    tc

  /**
   * Given two pomsets, generates fresh event ids for this pomset,
   * replacing accordingly, events, labels and orders
   * in order to avoid overlaping with events from the other pomset.
   * @param other pomset with whom to avoid overlap
   * @return same pomset with fresh event id's
   */
  def freshEvents(other:NPomset):NPomset =
    if events.intersect(other.events).isEmpty 
      then this
      else
        val max = (this.events ++ other.events).max
        val fresh:Map[Event,Event] = 
          this.events.zip(LazyList from (max+1)).toMap
        this.renameEvents(fresh)

  def renameEvents(rename:Map[Event,Event]):NPomset =
    val ne = events.map(e=>rename(e))
    val nl = labels.map({case(e,l)=>(rename(e),l)})
    val no = order.map(o =>Order(rename(o.left),rename(o.right)))
    val nn = nested.map(n=>n.map(p=>p.renameEvents(rename)))
    NPomset(ne,nl,no,nn,loop)

object NPomset:
  
  type Labels = Map[Event,SLabel]
  
  val identity:NPomset = NPomset(Set(),Map(),Set(),Set())

  def bigChoice(pomsets:Set[NPomset]):NPomset =
    NPomset(pomsets.flatMap(_.events).toSet
      , pomsets.flatMap(_.labels).toMap
      , pomsets.flatMap(_.order).toSet
      , Set(pomsets))

  def rmInChoice(poms:Set[NPomset],e:Event):Set[NPomset] =
    poms.map(p=>p-e)

  def rmInChoices(poms:Set[Set[NPomset]], e:Event):Set[Set[NPomset]] =
    poms.map(n=>rmInChoice(n,e))

  def eventsInChoice(poms:Set[NPomset]):Set[Event] =
    poms.flatMap(p=>p.events)

  def eventsInChoices(poms:Set[Set[NPomset]]):Set[Event] =
    poms.flatMap(n=>eventsInChoice(n))

  def findChoice(poms:Set[Set[NPomset]],e:Event):Option[Set[NPomset]] =
    poms.find(n=>eventsInChoice(n).contains(e))

  def findLocalInChoice(poms:Set[NPomset],e:Event):Option[NPomset] =
    val r = poms.find(p=>p.events.contains(e))
    if r.isDefined then
      if r.get.localEvents.contains(e) then r 
      else findLocalInChoice(r.get.nestedPom,e)
    else None 

  case class SLabel(act:Action):

    /**
     * Forced sequencing 
     * @param other pomset
     * @return a pomset where this action executes before anything in ther other pomset.
     */
    def ->(other:NPomset):NPomset =
      val e = if other.events.nonEmpty then other.events.max+1 else 0
      NPomset(Set(e)++other.events
        , other.labels++Map(e->this)
        , other.order++other.events.map(e1=>Order(e,e1)).toSet
        , other.nested,other.loop)
    
    def subj:Agent = act match
      case In(b,_,_) => b
      case Out(a,_,_) => a
      case _ => throw RuntimeException("Tau has no active agent") // tau to avoid 
    end subj 
    
    def agents:Set[Agent] = act match
      case In(a,b,_) => Set(a,b)
      case Out(a,b,_) => Set(a,b)
      case _ => Set() // tau to avoid warnings
    end agents