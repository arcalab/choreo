package choreo.choreo2.analysis.pomsets

import cats.{Always, Eval, Later, Now}
import cats.data.State
import cats.implicits._
import choreo.choreo2.analysis.Global
import choreo.choreo2.syntax.{Agent, Choreo, Msg}
import choreo.choreo2.analysis.pomsets.Pomset._
//import choreo.choreo2.analysis.pomsets.Pomset.LAct
//import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.view.MermaidPomset

object ChoreoPom:

  type St[A] = State[Int,A]
  
  def apply(c:Choreo):Pomset = pomsetOf(c).runA(0).value

  private def pomsetOf(c:Choreo):St[Pomset] = c match 
    case Send(as, bs, m) => 
      for
        ps <- as.flatTraverse(a => bs.traverse(b => send(a, b, m)))
      yield ps.foldRight(identity)(_>>_)
    case Seq(c1, c2) => 
      for
        p1 <- pomsetOf(c1)
        p2 <- pomsetOf(c2)
      yield p1 >> p2
    case Par(c1, c2) => 
      for
        p1 <- pomsetOf(c1)
        p2 <- pomsetOf(c2)
      yield p1 * p2
    case Choice(c1, c2) => 
      for
        p1 <- pomsetOf(c1)
        p2 <- pomsetOf(c2)
      yield p1 + p2
    case d@DChoice(c1, c2) => // todo: needs work
      //throw new RuntimeException("1-delayed choice not supported yet")
      val next:List[(Action,Choreo)] = Global.nextChoreo(d)
      //val p:Pomset =
      for 
        nextPoms <- next.traverse(n=> dchoice(n))
        //_ = nextPoms.map(p=>println(MermaidPomset(p)))
        m <- State.get
        //p = mkMChoice(nextPoms,e)
        _ <- State.set(m+1)
        e:Int = if nextPoms.flatMap(_.events).nonEmpty then nextPoms.flatMap(_.events).max+1 else m
      yield Pomset(nextPoms.flatMap(_.events).toSet+e,
        nextPoms.flatMap(_.labels).toMap+(e->LPoms(nextPoms.toSet)),
        nextPoms.flatMap(_.order).toSet++nextPoms.flatMap(_.events).map(e1=>Order(e,e1)).toSet+Order(e,e))
    case Loop(c) => 
      for 
        p <- pomsetOf(c)
      //yield p + identity
      yield identity + (p >> Pomset(p.events,p.labels,p.order,true))  
    case End => State.pure(identity)
    case In(b,a,m):Action => in(b,a,m)
    case Out(a,b,m): Action => out(a,b,m)
    case Tau:Action => State.pure(identity) //todo: check
  
  
  //private def mkMChoice(poms:List[Pomset],e:Event):Pomset = {
  //  var l:Labels = Map()
  //  for ((p,i) <- poms.zipWithIndex) do 
  //    l+= (e+i)->LPomp
  //  ???
  //}

  private def dchoice(next:(Action,Choreo)):St[Pomset] = for
    pn <- pomsetOf(next._2)
    //_ = println(s"[dchoice] for $next is ${pn} \n ${MermaidPomset(pn)}")
    e:Int <- State.get
    m:Int = if (pn.events).nonEmpty then pn.events.max+1 else e 
    _ <- State.set(e+2) 
  yield Pomset(Set(m,m+1)++pn.events,
    pn.labels++Map(m->LAct(next._1),(m+1)->LPoms(Set(pn))),
    pn.order++pn.events.map(e1=>Order(m+1,e1)).toSet+Order(m,m+1))
  
  private def send(from:Agent,to:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+2)
  yield Pomset(Set(e,e+1),Map(e->LAct(Out(from,to,m)),(e+1)->LAct(In(to,from,m))),Set(Order(e,e+1))) 
    
  private def out(from:Agent,to:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+1)
  yield Pomset(Set(e),Map(e->LAct(Out(from,to,m))),Set())

  private def in(to:Agent,from:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+1) 
  yield Pomset(Set(e),Map(e->LAct(In(to,from,m))),Set())