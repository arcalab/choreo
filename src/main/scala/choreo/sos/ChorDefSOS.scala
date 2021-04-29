package choreo.sos

//import choreo.lts.{Global, GlobalBasic, GlobalManyTaus}
import choreo.common.Simplify
import mat.sos.{SOS,WSOS}
import mat.sos.SOS._
import choreo.syntax.Choreo._
import choreo.syntax._

import scala.annotation.tailrec
import scala.sys.error

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


object ChorDefSOS extends WSOS[Action,Choreo]:
  override def next(c:Choreo): Set[(Action, Choreo)] = nextChoreo(c).toSet

  override def accepting(c: Choreo): Boolean = c match
    case _:Send => false
    case Seq(c1, c2) => accepting(c1) && accepting(c2)
    case Par(c1, c2) => accepting(c1) && accepting(c2)
    case Choice(c1, c2) => accepting(c1) || accepting(c2)
    case DChoice(c1, c2) => accepting(c1) || accepting(c2)
    case Loop(_) => true
    case End => true
    case Tau => false // makes more sense...
    case _: Action => false // NOT including tau

  /** SOS: next step of a Choreo expression */
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
          val c1a = agrees(c1,l).filter(p=> p._2!=c1) // filter to avoid repetation from nc2
          if c1a.nonEmpty then nagrees ++= c1a.map(p=> p._1->Simplify(p._2>c3))
        // --------------------------------------
        nc1.map(p=>p._1->Simplify(p._2>c2)) ++ // do c1
          nc2.map(p=>p._1->Simplify(c1>p._2)).filterNot(_._1==Tau) ++ // do c2
          nagrees ++// todo: check with Jose - do c2 if c1 agrees with
          (if accepting(c1) then nextChoreo(c2) else Nil) // add just c2 if c1 is final
      case Par(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1.map(p => p._1 -> Simplify(p._2||c2)) ++
          nc2.map(p => p._1 -> Simplify(c1||p._2))
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


  // c' = aggrees(c,a)  ==>  c --check a-> c'
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
      if (agents(a) intersect Set(a1,b1)).isEmpty then List((a,c))
      else List()
    case Send(a1::a1s, bs, m) => agrees(Send(List(a1),bs,m) > Send(a1s,bs,m),a)
    case Send(a1s, b::bs, m)  => agrees(Send(a1s,List(b),m) > Send(a1s,bs,m),a)
    case In(a1,_,m) =>
      if !(agents(a) contains a1) then List((a,c))
      else List()
    // only used for the evolution of projections
    case Out(a1,_,m) =>
      if !(agents(a) contains a1) then List((a,c))
      else List()
    case Tau =>
      List((a,c))
    case _ =>   error(s"Unknonwn agrees with $a for $c")
  }

  // checks if a choreo appears as a next step in a list of transitions
  def inNext(c:Choreo,t:List[(Action,Choreo)]):Boolean =
    t.find(s=>s._2==c).isDefined

  def nextPP(c:Choreo): String =
    SOS.nextPP(ChorDefSOS,c) // Global(c).transPP

  def nextSPP(c:Choreo, n:Int): String =
    goS(c,n).mkString("\n")

  /** Older version, to be replaced by nextS. */
  private def goS(c:Choreo,n:Int): Set[String] = n match
    case 0 => Set(s"~~> $c")
    case _ =>
      val nc = ChorDefSOS.next(c) //Global(c).trans
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




      
