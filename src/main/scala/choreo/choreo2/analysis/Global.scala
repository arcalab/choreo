package choreo.choreo2.analysis

import choreo.choreo2.backend.LTS
import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._

import scala.sys.error
import scala.annotation.tailrec
import choreo.choreo2.backend.Simplify._

// deprecated - can be dropped in favour of Choreo as instance of LTS.
//case class Global(c:Choreo) extends LTS[Global]:
//  override def get: Global = this
//
//  override def trans: Set[(Action, LTS[Global])] =
//    Global.nextChoreo(c).toSet.map(p=>(p._1,Global(p._2)))
//
//  override def accepting: Boolean = Global.canSkip(c)  

//case class Global(c:Choreo)
//  def get: Choreo = c

given LTS[Choreo]:
  extension (c:Choreo)
    def trans: Set[(Action,Choreo)] =
      Global.nextChoreo(c).toSet //.map(p=>(p._1,Global(p._2)))
    def accepting: Boolean = Global.canSkip(c)

object Global:

  /** SOS: next step of a Choreo expression */
//  def nextChoreo(c:Choreo)(using ignore: Set[Agent] = Set()): List[(Action,Choreo)] =
//    val nxt = c match
//      case Send(List(a), List(b), m) =>
//        if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
//      case Send(a::as, bs, m) => nextChoreo(Send(List(a),bs,m) || Send(as,bs,m))
//      case Send(as, b::bs, m) => nextChoreo(Send(as,List(b),m) || Send(as,bs,m))
//      case Seq(c1, c2) =>
//        val nc1 = nextChoreoNoTau(c1)
////        if nc1 == List((Tau,End)) then nextChoreo(c2)
////        else
//        val a1 = agents(c1)
//        val nc2 = nextChoreoNoTau(c2)(using ignore++a1)
//        nc1.map(p=>p._1->simple(p._2>c2)) ++
//          nc2.map(p=>p._1->simple(c1>p._2)) ++
//          (if canSkip(c1) then nextChoreoNoTau(c2) else Nil)
//      case Par(c1, c2) =>
//        val nc1 = nextChoreoNoTau(c1)
//        val nc2 = nextChoreoNoTau(c2)
//        nc1.map(p => p._1 -> simple(p._2||c2)) ++
//          nc2.map(p => p._1 -> simple(c1||p._2))
//      case Choice(c1, c2) =>
//        val nc1 = nextChoreoNoTau(c1)
//        val nc2 = nextChoreoNoTau(c2)
//        nc1 ++ nc2
//      case Loop(c2) =>
//        val nc2 = nextChoreoNoTau(c2)
//        nc2.map(p=>p._1 -> (p._2>c))
//      case End => Nil
//      case Tau => if ignore.isEmpty then List(Tau -> End) else Nil // not reachable
//      case In(a, b, m) =>
//        if ignore contains a then Nil else List(In(a,b,m) -> End)
//      case Out(a, b, m) =>
//        if ignore contains a then Nil else List(Out(a,b,m) -> End)
//      case _ => error("Unknonwn next for $c")
//    if canSkip(c) && ignore.isEmpty && c!=End // is final state, it is the front, and not 0.
//    then (Tau,End)::nxt
//    else nxt
//  
//  def nextChoreoNoTau(c:Choreo): Set[Agent] ?=> List[(Action,Choreo)] =
//    nextChoreo(c).filterNot(_._1==Tau)

  def nextChoreo(c:Choreo)(using ignore: Set[Agent] = Set()): List[(Action,Choreo)] =
    val nxt = c match
      case Send(List(a), List(b), m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
      case Send(a::as, bs, m) => nextChoreo(Send(List(a),bs,m) > Send(as,bs,m))
      case Send(as, b::bs, m) => nextChoreo(Send(as,List(b),m) > Send(as,bs,m))
      case Seq(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val a1 = agents(c1)
        val nc2 = nextChoreo(c2)(using ignore++a1)
        // todo: check with Jose ----------------
        var nagrees:List[(Action,Choreo)] = Nil 
        for ((l,c3) <- nextChoreo(c2)(using Set())) // todo check 
          val c1a = agrees(c1,l)
          if c1a.nonEmpty then nagrees ++= c1a.map(p=> p._1->simple(p._2>c3))
        // --------------------------------------
        nc1.map(p=>p._1->simple(p._2>c2)) ++ // do c1
          //nc2.map(p=>p._1->simple(c1>p._2)).filterNot(_._1==Tau) ++ // do c2 
          nagrees ++// todo: check with Jose - do c2 if c1 agrees with   
          (if canSkip(c1) then nextChoreo(c2) else Nil) // add just c2 if c1 is final
      case Par(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1.map(p => p._1 -> simple(p._2||c2)) ++
          nc2.map(p => p._1 -> simple(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        ///// comment last part to hide the rule c1+c2 -tau-> 0
        nc1 ++ nc2 //++ (if canSkip(c1)||canSkip(c2) then List(Tau -> End) else Nil)
      case DChoice(c1,c2) => //todo: check 
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
        val nj = ja.flatMap(a => group(a,nc1,nc2))
        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
        nj ++ ns 
      case Loop(c2) =>
        val nc2 = nextChoreo(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      case Tau => if ignore.isEmpty then List(Tau -> End) else Nil // not reachable
      case In(a, b, m) =>
        if ignore contains a then Nil else List(In(a,b,m) -> End)
      case Out(a, b, m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> End)
      case _ => error(s"Unknonwn next for $c")
    nxt

  def group(a:Action,nc1:List[(Action,Choreo)],nc2:List[(Action,Choreo)]):List[(Action,Choreo)] = 
    var joinedSteps:List[(Action,Choreo)] = List()
    for ((a1,c1) <- nc1; (a2,c2) <- nc2; if a1 == a && a2 == a) do  
      joinedSteps +:= ((a1,c1+c2))
    joinedSteps

  def agrees(c:Choreo,a:Action):List[(Action,Choreo)] = c match {
    case End => List((a,c))
    case Loop(c1) => 
      val t = agrees(c1,a)
      if inNext(c1,t) then List((a,c))
      else List((a,End))
    case Seq(c1,c2) =>  
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if (t1.isEmpty || t2.isEmpty) then List() 
      else for (nc1<- t1.map(_._2) ; nc2<- t2.map(_._2)) 
              yield (a,Seq(nc1,nc2))
    case Par(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if (t1.isEmpty || t2.isEmpty) then List()
        else for (nc1<- t1.map(_._2) ; nc2<- t2.map(_._2))
                yield (a,Seq(nc1,nc2))  
    case Choice(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if t1.isEmpty then t2 
      else if t2.isEmpty then t1
      else for (nc1<- t1.map(_._2) ; nc2<- t2.map(_._2))
        yield (a,Choice(nc1,nc2))
    case DChoice(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if t1.isEmpty then t2
      else if t2.isEmpty then t1
      else for (nc1<- t1.map(_._2) ; nc2<- t2.map(_._2))
        yield (a,DChoice(nc1,nc2))
    case Send(List(a1), List(b1), m) =>  
      if a!=a1 && a!=b1 then List((a,c))
      else List()
    case Send(a1::a1s, bs, m) => agrees(Send(List(a1),bs,m) > Send(a1s,bs,m),a)
    case Send(a1s, b::bs, m)  => agrees(Send(a1s,List(b),m) > Send(a1s,bs,m),a)
    case In(a1,b1,m) => 
      if a!=b1 then List((a,c))
      else List()
    case _ =>   error(s"Unknonwn agrees with $a for $c") 
  }

  // checks if a choreo appears as a next step in a list of transitions
  def inNext(c:Choreo,t:List[(Action,Choreo)]):Boolean =
    t.find(s=>s._2==c).isDefined
  
  def canSkip(c: Choreo): Boolean = c match
    case _:Send => false
    case Seq(c1, c2) => canSkip(c1) && canSkip(c2)
    case Par(c1, c2) => canSkip(c1) && canSkip(c2)
    case Choice(c1, c2) => canSkip(c1) || canSkip(c2)
    case DChoice(c1, c2) => canSkip(c1) || canSkip(c2)
    case Loop(_) => true
    case End => true
    case Tau => false // makes more sense...
    case _: Action => false // NOT including tau

  def nextPP(c:Choreo): String =
    c.transPP // Global(c).transPP

  def nextSPP(c:Choreo, n:Int): String =
    goS(c,n).mkString("\n")
  
  /** Older version, to be replaced by nextS. */
  private def goS(c:Choreo,n:Int): Set[String] = n match
    case 0 => Set(s"~~> $c")
    case _ =>
      val nc = c.trans //Global(c).trans
      nc.flatMap(p=> {
        val rec = goS(/*p._2.get.c*/ p._2,n-1)
        if rec.isEmpty then
          List(s"${p._1} [Done]")
        else {
          var fst = true
          val indent = " ".repeat(p._1.toString.length)+"   "
          for s <- rec
            yield s"${if fst then {fst=false; p._1.toString+" \\ "} else indent}$s"
        }
      })
  
//  def nextS(c:Choreo,n:Int): Set[(List[Action],Choreo)] = n match
//    case 0 => Set(Nil -> c)
//    case _ =>
//      val nc = c.trans //Global(c).trans
//      nc.flatMap(p=> {
//        val rec = nextS(p._2.get.c,n-1)
//        if rec.isEmpty then
//          List(List(p._1) -> End)
//        else
//          for s <- rec
//            yield (p._1::s._1) -> s._2
//      })

/// weak semantics, where Tau can only be taken at the beginning

  

case class GlobalManyTaus(c:Choreo):
  override def toString(): String = c.toString

given LTS[GlobalManyTaus]:
  extension (c:GlobalManyTaus)
    def accepting: Boolean = Global.canSkip(c.c)
    def trans: Set[(Action,GlobalManyTaus)] =
      GlobalManyTaus.nextChoreoTau(c.c).toSet.map(p=>(p._1,GlobalManyTaus(p._2)))

object GlobalManyTaus:
  def nextChoreoTau(c:Choreo)(using ignore:Set[Agent]=Set()): List[(Action,Choreo)] =
    val nxt = c match
      case Send(List(a), List(b), m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
      case Send(a::as, bs, m) => nextChoreoTau(Send(List(a),bs,m) || Send(as,bs,m))
      case Send(as, b::bs, m) => nextChoreoTau(Send(as,List(b),m) || Send(as,bs,m))
      case Seq(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
//        println(s"[NT] $c1 -> $nc1")
//        if nc1.toSet.map(_._1) == Set(Tau) then List((Tau,c2)) //nextChoreoTau(c2)
//        else
        val a1 = agents(c1)
        val nc2 = nextChoreoTau(c2)(using ignore++a1)
        nc1.map(p=>p._1->simple(p._2>c2)) ++  // seq 1 (c1 can go)
          nc2.map(p=>p._1->simple(c1>p._2)).filterNot(_._1==Tau) ++ // seq 2 (c2 can go if ignored agents)
          (if GlobalManyTaus(c1).accepting then nextChoreoTau(c2) else Nil) // seq3 (c2 can go if c1 accepting)
      case Par(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1.map(p => p._1 -> simple(p._2||c2)) ++
          nc2.map(p => p._1 -> simple(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1 ++ nc2 ++
          (if GlobalManyTaus(c1).accepting || GlobalManyTaus(c2).accepting
           then List(Tau -> End)
           else Nil)
      case DChoice(c1,c2) => // todo: check 
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
        val nj = ja.flatMap(a => Global.group(a,nc1,nc2))
        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
        nj ++ ns ++ 
          ( if GlobalManyTaus(c1).accepting || GlobalManyTaus(c2).accepting
            then List(Tau -> End)
            else Nil)
      case Loop(c2) =>
        val nc2 = nextChoreoTau(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      // tau can go if it is the first action
      case Tau => if ignore.isEmpty then List(Tau -> End) else Nil
      case In(a, b, m) =>
        if ignore contains a then Nil else List(In(a,b,m) -> End)
      case Out(a, b, m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> End)
      case _ => error(s"Unknonwn next for $c")
//    if GlobalTau(c).accepting && ignore.isEmpty && c!=End // is final state, it is the front, and not 0.
//    then (Tau,End)::nxt
//    else nxt
    nxt

      
case class GlobalBasic(c:Choreo):
  override def toString(): String = c.toString

given LTS[GlobalBasic]:
  extension (c:GlobalBasic)
    def accepting: Boolean = Global.canSkip(c.c)
    def trans: Set[(Action,GlobalBasic)] =
      GlobalBasic.nextChoreo(c.c).toSet.map(p=>(p._1,GlobalBasic(p._2)))

object GlobalBasic:
  def nextChoreo(c:Choreo)(using ignore: Set[Agent] = Set()): List[(Action,Choreo)] =
    val nxt = c match
      case Send(List(a), List(b), m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
      case Send(a::as, bs, m) => nextChoreo(Send(List(a),bs,m) > Send(as,bs,m))
      case Send(as, b::bs, m) => nextChoreo(Send(as,List(b),m) > Send(as,bs,m))
      case Seq(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val a1 = agents(c1)
        val nc2 = nextChoreo(c2)(using ignore++a1)
        nc1.map(p=>p._1->simple(p._2>c2)) ++ // do c1
          nc2.map(p=>p._1->simple(c1>p._2)).filterNot(_._1==Tau) ++ // do c2
          (if Global.canSkip(c1) then nextChoreo(c2) else Nil) // add just c2 if c1 is final 
      case Par(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1.map(p => p._1 -> simple(p._2||c2)) ++
          nc2.map(p => p._1 -> simple(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        ///// comment last part to hide the rule c1+c2 -tau-> 0
        nc1 ++ nc2 ++ (if Global.canSkip(c1)||Global.canSkip(c2) then List(Tau -> End) else Nil)
      case DChoice(c1,c2) => // todo: check 
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
        val nj = ja.flatMap(a => Global.group(a,nc1,nc2))
        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
        nj ++ ns ++
          ( if Global.canSkip(c1) || Global.canSkip(c2)
          then List(Tau -> End)
          else Nil)  
      case Loop(c2) =>
        val nc2 = nextChoreo(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      case Tau => if ignore.isEmpty then List(Tau -> End) else Nil // not reachable
      case In(a, b, m) =>
        if ignore contains a then Nil else List(In(a,b,m) -> End)
      case Out(a, b, m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> End)
      case _ => error(s"Unknonwn next for $c")
    nxt

      
