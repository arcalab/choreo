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
        nc1.map(p=>p._1->simple(p._2>c2)) ++ // do c1
          nc2.map(p=>p._1->simple(c1>p._2)) ++ // do c2
          (if canSkip(c1) then nextChoreo(c2) else Nil) // add just c2 if c1 is final 
      case Par(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1.map(p => p._1 -> simple(p._2||c2)) ++
          nc2.map(p => p._1 -> simple(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1 ++ nc2 //++
          //(if canSkip(c1)||canSkip(c2) then List(Tau -> End) else Nil)
      case Loop(c2) =>
        val nc2 = nextChoreo(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      // case Tau => if ignore.isEmpty then List(Tau -> End) else Nil // not reachable
      case In(a, b, m) =>
        if ignore contains a then Nil else List(In(a,b,m) -> End)
      case Out(a, b, m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> End)
      case _ => error("Unknonwn next for $c")
    nxt


  def canSkip(c: Choreo): Boolean = c match
    case _:Send => false
    case Seq(c1, c2) => canSkip(c1) && canSkip(c2)
    case Par(c1, c2) => canSkip(c1) && canSkip(c2)
    case Choice(c1, c2) => canSkip(c1) || canSkip(c2)
    case Loop(_) => true
    case End => true
    case Tau => true // experiment...
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

case class GlobalTau(c:Choreo):
  override def toString(): String = c.toString

given LTS[GlobalTau]:
  extension (c:GlobalTau)
    def accepting: Boolean = Global.canSkip(c.c)
    def trans: Set[(Action,GlobalTau)] =
      GlobalTau.nextChoreoTau(c.c).toSet.map(p=>(p._1,GlobalTau(p._2)))

object GlobalTau:
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
          nc2.map(p=>p._1->simple(c1>p._2)) ++ // seq 2 (c2 can go if ignored agents)
          (if GlobalTau(c1).accepting then nextChoreoTau(c2) else Nil) // seq3 (c2 can go if c1 accepting)
      case Par(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1.map(p => p._1 -> simple(p._2||c2)) ++
          nc2.map(p => p._1 -> simple(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1 ++ nc2 ++
          (if GlobalTau(c1).accepting || GlobalTau(c2).accepting
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
      case _ => error("Unknonwn next for $c")
//    if GlobalTau(c).accepting && ignore.isEmpty && c!=End // is final state, it is the front, and not 0.
//    then (Tau,End)::nxt
//    else nxt
    nxt

      
