package choreo.pomsets

import choreo.pomsets.NPomset._
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.{Action, In, Out, agents}

/**
 * Variation of the Pomset structure, using a nesting structure `N` that groups events.
 * @author José Proença
 */
case class NPomset(events: Nesting[Event],
                   actions: Actions,
                   order:Set[Order],
                   loop:Boolean=false):
  lazy val agents:Iterable[Agent] =
    actions.flatMap(kv => Choreo.agents(kv._2))

  /** Remove an event from the NPomset */
  def -(e:Event) = this -- Set(e)
  /** Remove a set of events from the NPomset */
  def --(es:Set[Event]):NPomset =
    NPomset(events--es,actions--es, order) // not dropping from the order, since it could break chains
//            order.filterNot(o=>es.contains(o.left) || es.contains(o.right)))

  /** Weak sequencing of NPomsets */
  def >>(other:NPomset): NPomset =
    val deps =
      for a <- other.agents
          in <- actions.filter(p=>active(a,p._2)).keys
          inOther <- other.actions.filter(p=>active(a,p._2)).keys
      yield Order(in,inOther)
    NPomset(events++other.events,actions++other.actions,order++other.order++deps)

  private def active(agent: Agent, act: Action) = act match
    case In(`agent`,_,_) => true
    case Out(`agent`,_,_) => true
    case _ => false

  /** Choice of NPomsets (ignoring loops so far) */
  def or(other:NPomset): NPomset =
    NPomset(events or other.events, actions++other.actions, order++other.order)

//  def refinements: Set[NPomset] =
//    (for choice <- events.cs do NPomset(Nesting())

  /** true if the event is not in a choice */
  def onTop(e:Event) = events.acts contains e

  /** Do minimum refinement until the pomset is ready to perform `e` */
  def readyFor(e:Event): Option[NPomset] =
    // 1. find if it has no predecessors (is minimum)
    // 2. for all predecessor, try to remove it by chosing empty choices (if available)
    // 3. if it is in a choice, remove alternatives.
    sys.error("not yet done")

  private lazy val maybePre:Map[Event,Set[Event]] =
    var res: Map[Event,Set[Event]] = Map()
    for Order(a,b)<-order do
      res += b -> (res.getOrElse(b,Set[Event]()) + a)
    res

  /** Calculate the real predecessors of an event, skiping over elements of the order not in the NPomset */
  def pre(e: Event): Set[Event] =
    maybePre(e).flatMap(e0=>if events.toSet contains e0 then Set(e0) else pre(e0))

  /** True if it has no events */
  def isEmpty = events.toSet.isEmpty


object NPomset:
  type Event = Int
  type Actions = Map[Event,Action]
  case class Order(left:Event,right:Event)
  case class Nesting[A](acts:Set[A], choices:Set[NChoice[A]]):
    lazy val toSet:Set[A] = acts ++ choices.flatMap(_.toSet)
    def --(as:Set[A]):Nesting[A] = Nesting(acts--as,choices.map(_--as))
    def ++(other:Nesting[A]): Nesting[A] = Nesting(acts++other.acts,choices++other.choices)
    def or(other:Nesting[A]): Nesting[A] = Nesting(Set(),Set(NChoice(this,other)))
  case class NChoice[A](left:Nesting[A],right:Nesting[A]):
    lazy val toSet:Set[A] = left.toSet ++ right.toSet
    def --(as:Set[A]):NChoice[A] = NChoice(left--as,right--as)

  def empty = NPomset(Nesting(Set(),Set()),Map(),Set(),false)
