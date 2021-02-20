package choreo.choreo2.analysis.pomsets

import cats.{Always, Eval, Later, Now}
import cats.data.State
import cats.implicits._
import choreo.choreo2.analysis.Global
import choreo.choreo2.analysis.Global.{group, nextChoreo}
import choreo.choreo2.syntax.{Agent, Choreo, Msg}
import choreo.choreo2.analysis.pomsets.Pomset._
//import choreo.choreo2.analysis.pomsets.Pomset.LAct
//import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.view.MermaidPomset

object ChoreoPom:

  private var seed:Int = 0
  private def next():Int = {seed+=1; seed-1}

  def apply(c:Choreo):Pomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p = ps.foldRight(identity)(_>>_)
      updateNext(p)
      p
    case Seq(c1, c2) =>
      val p = apply(c1) >> apply(c2)
      updateNext(p)
      p
    case Par(c1, c2) =>
      val p = apply(c1) * apply(c2)
      updateNext(p)
      p
    case Choice(c1, c2) =>
      val p = apply(c1) + apply(c2)
      updateNext(p)
      p
    case d@DChoice(c1, c2) => // todo: needs work
      //throw new RuntimeException("1-delayed choice not supported yet")
      //val nextChor:List[(Action,Choreo)] = Global.nextChoreo(d)
      //val nextPoms = nextChor.map(n=> dchoice(n))
      val nc1 = nextChoreo(c1)
      val nc2 = nextChoreo(c2)
      val ja = nc1.map(_._1).intersect(nc2.map(_._1))
      val nj = ja.flatMap(a => group(a,nc1,nc2))
      val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
      var nextPoms:List[Pomset] = List()
      if nj.nonEmpty then nextPoms = nj.map(dchoice)
      if ns.nonEmpty then nextPoms ++:= ns.map(dchoiceAlone)//(apply(Choice(c1,c2)))
      val e = next()
      var p:Pomset = identity 
      if (nextPoms.size == 1) then p = nextPoms.head
        else
          p = Pomset(nextPoms.flatMap(_.events).toSet+e,
            nextPoms.flatMap(_.labels).toMap+(e->LPoms(nextPoms.toSet)),
            nextPoms.flatMap(_.order).toSet++nextPoms.flatMap(_.events).map(e1=>Order(e,e1)).toSet+Order(e,e))
      updateNext(p)
      p
    case Loop(c) =>
      val pc =  apply(c)
      val p = identity + (pc >> Pomset(pc.events,pc.labels,pc.order,true))
      updateNext(p)
      p
    case End => identity
    case act@In(b,a,m):Action =>
      val e = next()
      Pomset(Set(e),Map(e->LAct(act)),Set())
    case act@Out(a,b,m): Action =>
      val e = next()
      Pomset(Set(e),Map(e->LAct(act)),Set())
    case Tau:Action => identity //todo: check


  private def dchoice(nextChor:(Action,Choreo)):Pomset =
    val pn =  apply(nextChor._2)
    val e1 = next()
    Pomset(Set(e1)++pn.events,  
      pn.labels++Map(e1->LAct(nextChor._1)),
      pn.order++pn.events.map(e=>Order(e1,e)).toSet)

  private def dchoiceAlone(nextChor:(Action,Choreo)):Pomset =
    val pn =  apply(nextChor._2)
    val e1 = next()
    val e2 = next()
    Pomset(Set(e1,e2)++pn.events,
      pn.labels++Map(e1->LAct(nextChor._1), e2->LPoms(Set(pn))),
      pn.order++pn.events.map(e=>Order(e2,e)).toSet+Order(e1,e2))
  

  private def send(from:Agent,to:Agent,m:Msg):Pomset =
    val e1 = next()
    val e2 = next()
    Pomset(Set(e1,e2),Map(e1->LAct(Out(from,to,m)),(e2)->LAct(In(to,from,m))),Set(Order(e1,e2)))

  private def updateNext(p:Pomset):Unit =
    if p.events.nonEmpty then seed = p.events.max.max(seed)+1