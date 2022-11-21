package choreo.pomsets

import choreo.pomsets._
import choreo.pomsets.Pomset._
import choreo.pomsets.Label._
import choreo.syntax.Choreo.Action
import caos.sos.SOS
type Act = choreo.syntax.Choreo
/**
 * Created by guillecledou on 18/03/2021
 * 
 * Global semantics for pomsets that removes executed events.
 */
object PomDefSOS extends SOS[Act,Pomset]:
  type PTrans[A] = Set[(A,Pomset)]

  override def next[A>:Act](p:Pomset): PTrans[A] = nextPom(p)
  override def accepting(p:Pomset):Boolean = isTerminating(p)

  def nextPom[A>:Act](p:Pomset):PTrans[A] =
    min(p).flatMap(e=>nextEvent(e,p.reduce))

  def nextPomPP(p:Pomset):String = SOS.nextPP(PomDefSOS,p)

  def nextEvent[A>:Act](e:Event,p:Pomset):PTrans[A] = p.labels(e) match
    case LPoms(pomsets) =>
      val valid = pomsets.flatMap(c=>nextOfChoice(p,e,c))
      valid.map(s=>(s._1,expand(s._2)))
    case LAct(act) =>
      Set((act,expand(p-e)))
  
  def nextOfChoice[A>:Act](from:Pomset, e:Event, to:Pomset):PTrans[A] =
    val evolved = takeChoice(from,e,to)
    val trans = nextPom[A](evolved)
    val minp = min(from) - e
    trans.filter(t => minp.subsetOf(min(t._2)))
  
  def takeChoice(from:Pomset,e:Event,to:Pomset):Pomset =
    val discard = from.labels(e).poms() - to
    (from-e) remove discard

  def isTerminating(p:Pomset):Boolean =
    p == Pomset.identity ||
      p.uniqueEvents.forall(e=> p.labels(e) match {
        case LPoms(pomsets) => pomsets.exists(p=>isTerminating(p))
        case LAct(act) => false
      })

  def isFinal(p:Pomset):Boolean =
    p == Pomset.identity ||
      p.labels.forall(l=>l._2.isFinal)

  def min(p:Pomset):Set[Event] =
    val r = p.reduce
    val minimal = r.uniqueEvents -- r.uniqueOrders.map(o=>o.right)
    //println(s"[MIN] of ${p} are\n\n: ${minimal.map(e=>p.labels(e))}")
    minimal 

  def expand(p:Pomset):Pomset =
    val nextNest = findNextNested(p)
    var pom = p
    for ((e,np) <- nextNest; pl <- np ; if pl.loop) do
      pom = expandLoop(pom,pl,e)
    end for
    pom

  def findNextNested(p:Pomset):Set[(Event,Set[Pomset])] =
    min(p).collect(e=>p.labels(e) match {case LPoms(ps) => (e,ps)})
  
  /**
   * Transform a loop pomset into Set(identity, (p >> p*)))
   * @param global
   * @param p
   * @return expanded pomset
   */
  def expandLoop(global:Pomset, p:Pomset,e:Event):Pomset =
    if !p.loop then p
    else // custom + and >> to avoide renaming 
      //val ep = p.freshEvents(global).encapsulate
      val (pl,rename) = replicateLoop(global,p)
      val ep = pl.encapsulate
      val seq = for a <- p.agents
                    in <- p.eventsOf(a)
                    inOther <- ep.eventsOf(a)
        yield Order(in,inOther)
      val outgoingOrders = remapOutgoingOrders(global,rename)
      val oneAndLoop = Pomset(p.events++ep.events
        , p.labels++ep.labels
        , p.order++ep.order++seq)
      Pomset(global.events++oneAndLoop.events
        , global.labels++oneAndLoop.labels + (e->LPoms(Set(oneAndLoop,Pomset.identity)))
        , global.order++oneAndLoop.order++outgoingOrders
        , global.loop)
  
  def replicateLoop(global:Pomset,pl:Pomset):(Pomset,Map[Event,Event]) =
    val max = (global.events ++ pl.events).max
    val fresh:Map[Event,Event] = pl.events.zip(LazyList from (max+1)).toMap
    (pl.renameEvents(fresh),fresh)
  
  def remapOutgoingOrders(global:Pomset,mapping:Map[Event,Event]):Set[Order] = 
    var orders = global.order 
    for (o <-global.order) 
      if mapping.keySet contains o.left then 
        orders -= o   
        orders += Order(mapping(o.left),mapping.getOrElse(o.right,o.right))
      end if   
    end for 
    orders
  

  def terminate(p:Pomset):Pomset =
    Pomset(p.events,p.labels.map(l=>l._1->LPoms(Set(Pomset.identity))),p.order)