package choreo.choreo2.analysis.pomsets

import choreo.choreo2.syntax.Choreo.Action
import choreo.choreo2.backend.LTS
import choreo.choreo2.analysis.pomsets.Pomset.{Event,Order}
import choreo.choreo2.analysis.pomsets.NPomset
import choreo.choreo2.analysis.pomsets.NPomset._

/**
 * Created by guillecledou on 23/03/2021
 */

object GlobalNPomset:

  type PTrans = Set[(Action,NPomset)]

  given globalPom as LTS[NPomset]:
    extension (p:NPomset)
      def trans: PTrans = nextPom(p)
      def accepting:Boolean = isAccepting(p)

  def nextPom(p:NPomset):PTrans = 
    val minimal = min(p)
    var next = minimal.map(e=>nextEvent(e,p.reduce))
    if (minimal.intersect(p.localEvents).isEmpty) && p.nested.exists(_.accepting) then 
      next ++=(p--p.nested).trans
    next
  

  def nextEvent(e:Event, p:NPomset):(Action,NPomset) =
    val pe = p.pomsetOf(e).get
    val a  = p.labels(e).act
    if pe == p then
      (a,p-e) //todo expand loop
    else
      val siblings = p.siblingsOf(pe)
      (a,p -- siblings - e) // todo expand loop

  def min(p:NPomset):Set[Event] =
    val r = p.reduce
    r.events -- r.order.map(o=>o.right)

  def isAccepting(p:NPomset):Boolean = 
    p == identity ||
      (p.localEvents.isEmpty &&
      (p.nested.isEmpty || p.nested.exists(p=>isAccepting(p)))) //p.nested.contains(identity)
  
