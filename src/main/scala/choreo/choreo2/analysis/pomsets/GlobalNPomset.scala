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
      def trans: PTrans =
        nextPom(p)
      def accepting:Boolean = isAccepting(p)

  //def nextPom(p:NPomset):PTrans =
  //  val minimal = min(p)
  //  var next = minimal.map(e=>nextEvent(e,p.reduce))
  //  if (minimal.intersect(p.localEvents).isEmpty) && p.nested.exists(_.accepting) then
  //    next ++=(p--p.nested).trans
  //  next
  def nextPom(p:NPomset):PTrans =
    val minimal = min(p)
    var next = minimal.map(e=>nextEvent(e,p.reduce))
    next ++= nextAdapted(p)
    //next++=adapted.flatMap(r=>nextPom(r))
    //
    //if (minimal.intersect(p.localEvents).isEmpty) && p.nested.exists(_.accepting) then
    //  next ++=(p--p.nested).trans
    next

  //def nextAdapted(p:NPomset):PTrans =
  //  println(s"[nextAdapted] of ${p}")
  //  //val poms = canAdaptTo(p)
  //  val minp = min(p)
  //  var next:PTrans = Set()
  //  println(s"[Nested] are ${p.nestedPom}")
  //  for (c<-p.nested; r<-c)
  //  //for (c<-p.nested; rn<-c ; r<-rn.nestedPom+rn)
  //    val adapted = adapt(p,c,r) 
  //    println(s"[nextAdapted] chose ${r}")
  //    println(s"[nextAdapted] adapted to ${adapted}")
  //    val minimal = min(adapted) -- (if isAccepting(r) then Set() else minp)
  //    println(s"[nextAdapted] minimals ${min(adapted)} but without known ${minimal}")
  //    next ++= minimal.map(e=>nextEvent(e,adapted.reduce))
  //  next


  def nextAdapted(p:NPomset):PTrans =
    println(s"[nextAdapted] of ${p}")
    //val poms = canAdaptTo(p)
    val minp = min(p)
    var next:PTrans = Set()
    println(s"[Nested] are ${p.nestedPom}")
    for (c<-p.nested; r<-c)
      //for (c<-p.nested; rn<-c ; r<-rn.nestedPom+rn)
      val adapted = adapt(p,c,r)
      println(s"[nextAdapted] chose ${r}")
      println(s"[nextAdapted] adapted to ${adapted}")
      //val minimal = min(adapted) -- (if isAccepting(r) then Set() else minp)
      //println(s"[nextAdapted] minimals ${min(adapted)} but without known ${minimal}")
      //next ++= minimal.map(e=>nextEvent(e,adapted.reduce))
      val t = nextPom(adapted)
      val vt = t.filter(t1=>min(t1._2))
      println(s"[nextAdaptede] new t = ${t.mkString("\n")}")
      next ++= t
    next
  //def nextEvent(e:Event, p:NPomset):(Action,NPomset) =
  //  val pe = p.pomsetOf(e).get
  //  val a  = p.labels(e).act
  //  if pe == p then
  //    (a,p-e) //todo expand loop
  //  else
  //    val siblings = p.siblingsOf(pe)
  //    (a,p -- siblings - e) // todo expand loop

  def nextEvent(e:Event, p:NPomset):(Action,NPomset) =
    val a  = p.labels(e).act
    (a,p-e) //todo expand loop

  //def canAdaptTo(p:NPomset):Set[NPomset] = {
  //  println(s"[Nested] are ${p.nestedPom}")
  //  for (n<-p.nested; r<-n) 
  //    yield adapt(p,n,r)
  //  //for (r <- p.nestedPom; if min(r).nonEmpty || r==NPomset.identity)
  //  //  yield r
  //}


  def adapt(p:NPomset,c:Set[NPomset],r:NPomset):NPomset =
    val ne = p.events -- NPomset.eventsInChoice(c-r)
    val nl = p.labels.filter(l=>ne.contains(l._1))
    val no = p.order.filter(o=>ne.contains(o.left) && ne.contains(o.right))
    val nn = (p.nested - c) ++ r.nested 
    NPomset(ne,nl,no,nn,p.loop)

  def min(p:NPomset):Set[Event] =
    val r = p.reduce
    r.events -- r.order.map(o=>o.right)

  def isAccepting(p:NPomset):Boolean = 
    p == identity ||
      (p.localEvents.isEmpty &&
       (p.nested.exists(n=>n.exists(p=>isAccepting(p)))))//n.exists(p=>isAccepting(p))))) //p.nested.contains(identity)
  
