package choreo.sos

import choreo.common.Simplify
import choreo.sos.ChorDefSOS
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.{Action, Choice, DChoice, End, In, Loop, Out, Par, Send, Seq, Tau, agents}
import mat.sos.SOS
import mat.sos.SOS._

import scala.sys.error

object ChorManyTausSOS extends SOS[Action,Choreo] :
  override def accepting(c:Choreo): Boolean = ChorDefSOS.accepting(c)
  override def next(c:Choreo): Set[(Action, Choreo)] = nextChoreoTau(c).toSet

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
        nc1.map(p=>p._1->Simplify(p._2>c2)) ++  // seq 1 (c1 can go)
          nc2.map(p=>p._1->Simplify(c1>p._2)).filterNot(_._1==Tau) ++ // seq 2 (c2 can go if ignored agents)
          (if accepting(c1) then nextChoreoTau(c2) else Nil) // seq3 (c2 can go if c1 accepting)
      case Par(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1.map(p => p._1 -> Simplify(p._2||c2)) ++
          nc2.map(p => p._1 -> Simplify(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        nc1 ++ nc2 ++
          (if accepting(c1) || accepting(c2)
          then List(Tau -> End)
          else Nil)
      case DChoice(c1,c2) => // todo: check
        val nc1 = nextChoreoTau(c1)
        val nc2 = nextChoreoTau(c2)
        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
        val nj = ja.flatMap(a => ChorDefSOS.group(a,nc1,nc2))
        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
        nj ++ ns ++
          ( if accepting(c1) || accepting(c2)
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
