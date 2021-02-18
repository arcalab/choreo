package choreo.choreo2.analysis.pomsets

import choreo.choreo2.view.DotPomsets
import choreo.choreo2.view.DotPomsets.dotPomset
import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.backend.LTS
import choreo.choreo2.syntax.Choreo.Action

/**
 * Created by guillecledou on 02/02/2021
 */

object GlobalPom:
  type PTrans = Set[(Action,Pomset)]
  
  given globalPom as LTS[Pomset]:
    extension (p:Pomset)
      def trans: PTrans = nextPom(p) 
      def accepting:Boolean = isTerminating(p)
  
  def nextPom(p:Pomset):PTrans =
    val minAlive = min(p) 
    minAlive.flatMap(e=>nextEvent(e,p.reduce))
  
  def nextPomPP(p:Pomset):String = p.transPP
  
  // old evolution (too permisive with choice)
  //def nextEvent(e:Event,p:Pomset):PTrans = p.labels(e) match
  //  case LPoms(pomsets) =>
  //      val expanded = pomsets.flatMap(pl=>expand(p,pl))
  //      val ne = p.events ++ expanded.flatMap(p=>p.events)
  //      val nl = p.labels.updated(e,LPoms(expanded)) ++ expanded.flatMap(p=>p.labels)
  //      val no = p.order++expanded.flatMap(p=>p.order)
  //      val np = Pomset(ne,nl,no,p.loop).reduce
  //      expanded.flatMap(pom=>nextPom(updateWithChoice(e,np,expanded-pom)))
  //  case LAct(act) =>
  //    val np = Pomset(p.events,p.labels.updated(e,LPoms(Set(Pomset.identity))),p.order,p.loop)
  //    Set((act,np))

  //def updateWithChoice(e:Event, from:Pomset, others:Set[Pomset]):Pomset =
  //  val terminate = others.flatMap(_.events) + e 
  //  val termLabel = terminate.map(e => (e,LPoms(Set(Pomset.identity)))).toMap
  //  val newLabels = from.labels ++ termLabel
  //  Pomset(from.events,newLabels,from.order)
  
  def nextEvent(e:Event,p:Pomset):PTrans = p.labels(e) match
    case LPoms(pomsets) =>
      val nextsInChoice:Set[(Pomset,PTrans)] = pomsets.map(pom => (pom,nextPom(pom)))
      val steps = nextsInChoice.flatMap(p1=> 
        p1._2.map(t => (t._1,updateWithChoice(e,p,t._2,pomsets-p1._1))))
      steps.map(s => (s._1,expand(s._2)))
    case LAct(act) =>
      val np = Pomset(p.events,p.labels.updated(e,LPoms(Set(Pomset.identity))),p.order,p.loop)
      Set((act,expand(np)))

  def updateWithChoice(e:Event, from:Pomset, sel:Pomset,others:Set[Pomset]):Pomset =
    val terminate = others.flatMap(_.events) + e
    val termLabel = terminate.map(e => (e,LPoms(Set(Pomset.identity)))).toMap
    val newLabels = from.labels ++ termLabel ++ sel.labels
    Pomset(from.events,newLabels,from.order,from.loop)
  
  def isTerminating(p:Pomset):Boolean = 
    p == Pomset.identity || 
    min(p).forall(e=> p.labels(e) match {
        case LPoms(pomsets) => pomsets.exists(p=>isTerminating(p))
        case LAct(act) => false
      })
      
  def isFinal(p:Pomset):Boolean =
    p == Pomset.identity || 
      p.labels.forall(l=>l._2.isFinal)

  def min(p:Pomset):Set[Event] =
    val r = p.reduce
    val finale = r.uniqueEvents.filter(e=>r.labels(e).isFinal)
    (r.uniqueEvents -- r.uniqueOrders.filterNot(o=> finale contains o.left).map(o=>o.right)) -- finale
    //(r.uniqueOrders.map(o=>o.left) -- (r.uniqueOrders.map(o=>o.right))) intersect uniqueEvents
  
  
  def expand(p:Pomset):Pomset =
    val nextNest = findNextNested(p)
    var pom = p 
    for ((e,np) <- nextNest; pl <- np ; if pl.loop) do
      pom = expandLoop(pom,pl,e)
    pom
    
  def findNextNested(p:Pomset):Set[(Event,Set[Pomset])] =
    min(p).collect(e=>p.labels(e) match {case LPoms(ps) => (e,ps)})

  //def expandEvent(pomsets:Set[Pomset],p:Pomset):Set[Pomset] = 
  //  pomsets.flatMap(pl=>expandLoop(p,pl))    
  
  /**
   * Transform a loop pomset into Set(identity, (p >> p*)))
   * @param global
   * @param p
   * @return expanded pomset
   */ 
  def expandLoop(global:Pomset, p:Pomset,e:Event):Pomset =
    if !p.loop then p
    else // custom + and >> to avoide renaming 
      val ep = p.freshEvents(global).encapsulate
      val seq = for a <- p.agents
                    in <- p.eventsOf(a)
                    inOther <- ep.eventsOf(a)
        yield Order(in,inOther)
      val oneAndLoop = Pomset(p.events++ep.events,p.labels++ep.labels,p.order++ep.order++seq)
      Pomset(global.events++oneAndLoop.events,
        global.labels++oneAndLoop.labels + (e->LPoms(Set(oneAndLoop,Pomset.identity))),
        global.order++oneAndLoop.order,global.loop)
    
    
  def terminate(p:Pomset):Pomset =
    Pomset(p.events,p.labels.map(l=>l._1->LPoms(Set(Pomset.identity))),p.order)
  
      

