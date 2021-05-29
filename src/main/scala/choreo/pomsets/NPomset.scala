package choreo.pomsets

import choreo.pomsets.NPomset._
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.syntax.Choreo.{Action, In, Out, agents}

/**
 * Variation of the Pomset structure, using a nesting structure `N` that groups events.
 * It is not kept normalised, i.e., the mapping of `actions` and `order` may refer to
 * non-existing events in the `event` nested set.
 *
 * So far, ignoring loops and delayed choices.
 * @author José Proença
 */
case class NPomset(events: Events,
                   actions: Actions,
                   pred:Order,
                   loopSucc:Order):
  lazy val agents:Iterable[Agent] =
    actions.flatMap(kv => Choreo.agents(kv._2))

  /** Remove an event from the NPomset */
  def -(e:Event) = this -- Set(e)
  /** Remove a set of events from the NPomset */
  def --(es:Set[Event]):NPomset =
    NPomset(events--es,actions--es, pred, loopSucc) // not dropping from the order, since it could break chains
//            order.filterNot(o=>es.contains(o.left) || es.contains(o.right)))

  /** Weak sequencing of NPomsets */
  def >>(other:NPomset): NPomset =
    val deps =
      for a <- other.agents
          in <- actions.filter(p=>isActive(a,p._2)).keys
          inOther <- other.actions.filter(p=>isActive(a,p._2)).keys
      yield (inOther,in)
    NPomset(events++other.events,actions++other.actions,
      add(deps,add(pred,other.pred)), add(loopSucc,other.loopSucc))

  private def isActive(agent: Agent, act: Action) = act match
    case In(`agent`,_,_) => true
    case Out(`agent`,_,_) => true
    case _ => false

  /** Choice of NPomsets (ignoring loops so far) */
  def or(other:NPomset): NPomset =
    NPomset(events or other.events, actions++other.actions,
      add(pred,other.pred), add(loopSucc,other.loopSucc))

  /** Parallel composition of NPomsets */
  def ++(other: NPomset): NPomset =
    NPomset(events ++ other.events, actions++other.actions,
      add(pred,other.pred), add(loopSucc,other.loopSucc))

  //  def refinements: Set[NPomset] =
//    (for choice <- events.cs do NPomset(Nesting())

  ///////////////
  // Refinement functions to be used in the semantics
  ///////////////

  /** true if the event is not in a choice */
  def onTop(e:Event) = events.acts contains e

  /** Do minimum refinement until the pomset is ready to perform `e` */
  def readyFor(e:Event): Option[NPomset] =
    for
    // 1. for all predecessor, try to remove it by chosing empty choices (if available)
      evs1 <- dropEvents(allRealPred(e),events)
    // 2. if it is in a choice, remove alternatives.
      evs2 <- select(e,evs1)
    yield
      NPomset(evs2,actions,pred,loopSucc)

//  private lazy val maybePre:Map[Event,Set[Event]] =
//    var res: Map[Event,Set[Event]] = Map()
//    for Order(a,b)<-order do
//      res += b -> (res.getOrElse(b,Set[Event]()) + a)
//    res.withDefaultValue(Set())

  /** Calculate the real predecessors of an event, skiping over elements of the order not in the NPomset */
  def realPred(e: Event): Set[Event] =
   pred.getOrElse(e,Set())
     .flatMap(e0=>if events.toSet contains e0 then Set(e0) else realPred(e0))

  /** Calculate ALL real predecessors of an event, skiping over elements of the order not in the NPomset */
  def allRealPred(e: Event): Set[Event] =
    val next = realPred(e)
    next ++ next.flatMap(allRealPred)

  /** Refines (minimally) a nested set of events to drop a set of events.
   * Returne None if the events cannot be dropped. */
  def dropEvents(es:Set[Event],n:Events): Option[Events] =
    if n.acts.intersect(es).nonEmpty then None
    else
      val newChoices:Set[Events] = for c <- n.choices yield
        (dropEvents(es,c.left),dropEvents(es,c.right)) match
          case (None,None) => return None // no need to continue
          case (Some(a),None) => a
          case (None,Some(b)) => b
          case (Some(a),Some(b)) => Nesting(Set(),Set(NChoice(a,b)),Set())
      val jointChoices = newChoices.fold(Nesting(n.acts,Set(),Set()))(_++_)
      Some(jointChoices)

  /** Refines (minimally) a nested set of events to lift a given event up.
   * Returns None if the event is not found. */
  def select(event: NPomset.Event,n:Events): Option[Events] =
    if n.acts.contains(event)
      then Some(n)
    else
      var found=false
      val newChoices = for c <- n.choices yield
        (select(event,c.left),select(event,c.right)) match
          case (None,None) => Nesting(Set(),Set(c),Set())
          case (Some(a),_) => {found=true; a}
          case (_,Some(b)) => {found=true; b}
      if !found then None
      else Some(newChoices.fold(Nesting(n.acts,Set(),Set()))(_++_))

  def accepting: Boolean = canTerminate(events)
  private def canTerminate(es: Events): Boolean =
    es.acts.isEmpty && es.choices.forall(canTerminate)
  private def canTerminate(ch: NChoice[Event]): Boolean =
    canTerminate(ch.left) || canTerminate(ch.right)

  //////////////////
  // Auxiliary
  //////////////////
  override def toString: String =
//    val evs = events.toSet
    val sEv = pretty(events)
    val sAct = actions.map((a,b)=>s"$a:$b").mkString(",")
    val sOrd = (for ((a,bs)<-pred; b<-bs) yield s"$b<$a").mkString(",")
    val sLoop = (for ((a,bs)<-loopSucc;b<-bs) yield s"$a'<$b").mkString(",")
    List(sEv,sAct,sOrd,sLoop).filterNot(_=="").mkString(" | ")

  private def pretty(e:Events): String =
    (e.acts.map(_.toString) ++
      e.choices.map(c => s"[${pretty(c.left)}+${pretty(c.right)}]") ++
      e.loops.map(l=> s"(${pretty(l)})*"))
      .mkString(",")

  /** True if it has no events */
  def isEmpty = events.toSet.isEmpty


object NPomset:
  type Event = Int
  type Actions = Map[Event,Action]
  type Order = MS[Event,Event] // Map[Event,Set[Event]]
  type Events = Nesting[Event]

  /** Isomorphic to List[(A,B)], indexed on A */
  type MS[A,B] = Map[A,Set[B]]
  def mapset[A,B](a:A,b:B)=Map(a->Set(b))
  def add[A,B](ab:(A,B),m:MS[A,B]): MS[A,B] =
    val (a,b) = ab
    if m contains a then m+(a->(m(a)+b)) else m+(a->Set(b))
  def add[A,B](abs:Iterable[(A,B)],m:MS[A,B]): MS[A,B] =
    abs.foldRight(m)((ab,prev)=> add(ab,prev))
  def add[A,B](m1:MS[A,B], m2:MS[A,B]): MS[A,B] =
    m1 ++ (for (a,bs)<-m2 yield
      if m1 contains a then a->(m1(a)++bs) else a->bs)

  case class Nesting[A](acts:Set[A], choices:Set[NChoice[A]],loops:Set[Nesting[A]]):
    lazy val toSet:Set[A] = acts ++ choices.flatMap(_.toSet)
    def --(as:Set[A]):Nesting[A] = Nesting(acts--as,choices.map(_--as),loops)
    def ++(other:Nesting[A]): Nesting[A] = Nesting(acts++other.acts,choices++other.choices,loops++other.loops)
    def or(other:Nesting[A]): Nesting[A] = Nesting(Set(),Set(NChoice(this,other)),Set())

  case class NChoice[A](left:Nesting[A],right:Nesting[A]):
    lazy val toSet:Set[A] = left.toSet ++ right.toSet
    def --(as:Set[A]):NChoice[A] = NChoice(left--as,right--as)

  def empty = NPomset(Nesting(Set(),Set(),Set()),Map(),Map(),Map())

  val nex = Nesting(Set(1,2,3),Set(NChoice(Nesting(Set(4,5),Set(),Set()),Nesting(Set(6,7),Set(),Set()))),Set()) // 1,2,3,[4,5+6,7]
  val pex = NPomset(nex,Map(1->Out(Agent("a"),Agent("b")),4->In(Agent("b"),Agent("a"))), add((2,1),mapset(4,3)), Map())
  import choreo.Examples._
  val ex2 = Choreo2NPom(((a->d) + (b->d)) > (a->d))
