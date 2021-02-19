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
    case d@DChoice(c1, c2) => 
      throw new RuntimeException("1-delayed choice not supported yet")
      //val next:List[(Action,Choreo)] = Global.nextChoreo(d)
      ////val p:Pomset =
      //for 
      //  nextsPoms <- next.traverse(n=> dchoice(n))
      //  p = nextsPoms.foldRight[Pomset](identity)(_+_)
      //yield p    
    case Loop(c) => 
      for //todo: for now one iteration or nothing (check)
        p <- pomsetOf(c)
      //yield p + identity
      yield identity + (p >> Pomset(p.events,p.labels,p.order,true))  
    case End => State.pure(identity)
    case In(a,b,m):Action => in(b,a,m)
    case Out(a,b,m): Action => out(a,b,m)
    case Tau:Action => State.pure(identity) //todo: check
  
  private def dchoice(next:(Action,Choreo)):St[Pomset] = for 
    pa <- pomsetOf(Seq(next._1, next._2))
  yield pa  
  
  private def send(from:Agent,to:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+2)
  yield Pomset(Set(e,e+1),Map(e->LAct(Out(from,to,m)),(e+1)->LAct(In(from,to,m))),Set(Order(e,e+1))) 
    
  private def out(from:Agent,to:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+1)
  yield Pomset(Set(e),Map(e->LAct(Out(from,to,m))),Set())

  private def in(to:Agent,from:Agent,m:Msg):St[Pomset] = for
    e <- State.get
    _ <- State.set(e+1) 
  yield Pomset(Set(e),Map(e->LAct(In(from,to,m))),Set())