package choreo.choreo2.analysis.pomsets

import choreo.choreo2.view.DotPomsets
import choreo.choreo2.view.DotPomsets.dotPomset
import choreo.choreo2.analysis.pomsets.Pomset.{Event, LAct, LPoms, Label, Order, identity}
import choreo.choreo2.backend.LTS
import choreo.choreo2.syntax.Choreo.Action

/**
 * Created by guillecledou on 02/02/2021
 */

object GlobalPom:

  given globalPom as LTS[Pomset]:
    extension (p:Pomset)
      def trans: Set[(Action,Pomset)] = nextPom(p) 
      def accepting:Boolean = isTerminating(p)
  
  def nextPom(p:Pomset):Set[(Action,Pomset)] =
    val minAlive = min(p) 
    minAlive.flatMap(e=>nextEvent(e,p.reduce))
  
  def nextEvent(e:Event,p:Pomset):Set[(Action,Pomset)] = p.labels(e) match 
    case LPoms(pomsets) =>
        val expanded = pomsets.flatMap(pl=>expand(p,pl))
        val ne = p.events ++ expanded.flatMap(p=>p.events)
        val nl = p.labels.updated(e,LPoms(expanded)) ++ expanded.flatMap(p=>p.labels)
        val no = p.order++expanded.flatMap(p=>p.order)
        val np = Pomset(ne,nl,no,p.loop).reduce
        expanded.flatMap(pom=>nextPom(updateWithChoice(e,np,expanded-pom)))
    case LAct(act) =>
      val np = Pomset(p.events,p.labels.updated(e,LPoms(Set(Pomset.identity))),p.order,p.loop) 
      Set((act,np))

  def updateWithChoice(e:Event, from:Pomset, others:Set[Pomset]):Pomset =
    val terminate = others.flatMap(_.events) + e 
    val termLabel = terminate.map(e => (e,LPoms(Set(Pomset.identity)))).toMap
    val newLabels = from.labels ++ termLabel
    Pomset(from.events,newLabels,from.order)
  
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
  
  //def terminating(p:Pomset):Set[Event] =
  //  p.min.filter(e=> p.labels(e) match {
  //    case LPoms(pomsets) => pomsets.exists(p=>isTerminating(p))
  //    case LAct(act) => false
  //  })

  def nextPomPP(p:Pomset):String = p.transPP
  
  
  /**
   * Transform a loop pomset into Set(identity, (p >> p*)))
   * @param global
   * @param p
   * @return expanded pomset
   */ 
  def expand(global:Pomset, p:Pomset):Set[Pomset] =
    if !p.loop then Set(p)
    else // custom + and >> to avoide renaming 
      val ep = p.freshEvents(global).encapsulate
      val seq = for a <- p.agents
                    in <- p.eventsOf(a)
                    inOther <- ep.eventsOf(a)
        yield Order(in,inOther)
      //val max = ep.events.max+1
      val oneAndLoop = Pomset(p.events++ep.events,p.labels++ep.labels,p.order++ep.order++seq)
      Set(oneAndLoop,identity)
      //Pomset(oneAndLoop.events++Set(max),
      //  oneAndLoop.labels++Map(max->LPoms(Set(oneAndLoop,identity))),
      //  oneAndLoop.order++(oneAndLoop.events).map(e1=>Order(max,e1))+Order(max,max))
    
    
  def terminate(p:Pomset):Pomset =
    Pomset(p.events,p.labels.map(l=>l._1->LPoms(Set(Pomset.identity))),p.order)
  
      

