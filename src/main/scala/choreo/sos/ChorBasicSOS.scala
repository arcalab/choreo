package choreo.sos

import mat.sos.SOS
import choreo.common.Simplify
import choreo.sos.ChorDefSOS
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

import scala.sys.error


case class ChorBasicSOS(c:Choreo):
  override def toString(): String = c.toString

object ChorBasicSOS extends SOS[Action,Choreo]:
  override def accepting(s: Choreo): Boolean = ChorDefSOS.accepting(s)
  override def next(c: Choreo): Set[(Action, Choreo)] = nextAux(c).toSet

  private def nextAux(c:Choreo)(using ignore: Set[Agent] = Set()): List[(Action,Choreo)] =
    val nxt = c match
      case Send(List(a), List(b), m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
      case Send(a::as, bs, m) => nextAux(Send(List(a),bs,m) > Send(as,bs,m))
      case Send(as, b::bs, m) => nextAux(Send(as,List(b),m) > Send(as,bs,m))
      case Seq(c1, c2) =>
        val nc1 = nextAux(c1)
        val a1 = agents(c1)
        val nc2 = nextAux(c2)(using ignore++a1)
        nc1.map(p=>p._1->Simplify(p._2>c2)) ++ // do c1
          nc2.map(p=>p._1->Simplify(c1>p._2)).filterNot(_._1==Tau) ++ // do c2
          (if ChorDefSOS.accepting(c1) then nextAux(c2) else Nil) // add just c2 if c1 is final
      case Par(c1, c2) =>
        val nc1 = nextAux(c1)
        val nc2 = nextAux(c2)
        nc1.map(p => p._1 -> Simplify(p._2||c2)) ++
          nc2.map(p => p._1 -> Simplify(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextAux(c1)
        val nc2 = nextAux(c2)
        ///// comment last part to hide the rule c1+c2 -tau-> 0
        nc1 ++ nc2 ++ (if ChorDefSOS.accepting(c1)||ChorDefSOS.accepting(c2) then List(Tau -> End) else Nil)
      case DChoice(c1,c2) => // todo: check
        val nc1 = nextAux(c1)
        val nc2 = nextAux(c2)
        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
        val nj = ja.flatMap(a => ChorDefSOS.group(a,nc1,nc2))
        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
        nj ++ ns ++
          ( if ChorDefSOS.accepting(c1) || ChorDefSOS.accepting(c2)
          then List(Tau -> End)
          else Nil)
      case Loop(c2) =>
        val nc2 = nextAux(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      case Tau => if ignore.isEmpty then List(Tau -> End) else Nil // not reachable
      case In(a, b, m) =>
        if ignore contains a then Nil else List(In(a,b,m) -> End)
      case Out(a, b, m) =>
        if ignore contains a then Nil else List(Out(a,b,m) -> End)
      case _ => error(s"Unknonwn next for $c")
    nxt


