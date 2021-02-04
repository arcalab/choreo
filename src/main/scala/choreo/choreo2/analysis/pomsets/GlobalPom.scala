package choreo.choreo2.analysis.pomsets

import choreo.choreo2.analysis.pomsets.Pomset.{Event, LAct, LPoms, Label}
import choreo.choreo2.backend.LTS
import choreo.choreo2.syntax.Choreo.Action

/**
 * Created by guillecledou on 02/02/2021
 */

object GlobalPom:

  given LTS[Pomset]:
    extension (p:Pomset)
      def trans: Set[(Action,Pomset)] = nextPom(p) 
      def accepting:Boolean = isTerminating(p)
  
  def nextPom(p:Pomset):Set[(Action,Pomset)] =
    val minAlive = min(p) 
    minAlive.flatMap(e=>nextEvent(e,p.reduce))
  
  def nextEvent(e:Event,p:Pomset):Set[(Action,Pomset)] = p.labels(e) match 
    case LPoms(pomsets) =>
        pomsets.flatMap(pom=>nextPom(updateWithChoice(e,p,pomsets-pom)))
    case LAct(act) =>
      val np = Pomset(p.events,p.labels.updated(e,LPoms(Set(Pomset.identity))),p.order) 
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