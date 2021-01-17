package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import scala.sys.error
import scala.annotation.tailrec

import choreo.choreo2.backend.Simplify._

object SOS:
  def nextPP(c:Choreo): String =
    val nc = nextChoreo(c)(Set())
    nc.map(p=>s"${p._1} ~~> ${p._2}").mkString("\n")

  def nextSPP(c:Choreo, n:Int): String =
    goS(c,n).mkString("\n")

  /** Older version, to be replaced by nextS. */
  private def goS(c:Choreo,n:Int): List[String] = n match
    case 0 => List(s"~~> $c")
    case _ =>
      val nc = nextChoreo(c)(Set())
      nc.flatMap(p=> {
        val rec = goS(p._2,n-1)
        if rec.isEmpty then
          List(s"${p._1} [Done]")
        else {
          var fst = true
          val indent = " ".repeat(p._1.toString.length)+"   "
          for s <- rec
            yield s"${if fst then {fst=false; p._1.toString+" \\ "} else indent}$s"
        }
      })

  def nextS(c:Choreo,n:Int): List[(List[Action],Choreo)] = n match
    case 0 => List(Nil -> c)
    case _ =>
      val nc = nextChoreo(c)(Set())
      nc.flatMap(p=> {
        val rec = nextS(p._2,n-1)
        if rec.isEmpty then
          List(List(p._1) -> End)
        else
          for s <- rec
            yield (p._1::s._1) -> s._2
      })

  // non-used experiments:
  def choicesG(c:Choreo): List[Action] = nextChoreo(c).map(_._1)
  def choicesL(c:Choreo): String =
    allProj(c).view.mapValues(choicesG).mkString("\n")

  /** SOS: next step of a Choreo expression */
  def nextChoreo(c:Choreo)(implicit ignore:Set[Agent]=Set()): List[(Action,Choreo)] = c match
    case Send(List(a), List(b), m) =>
      if ignore contains a then Nil else List(Out(a,b,m) -> In(b,a,m))
    case Send(a::as, bs, m) => nextChoreo(Send(List(a),bs,m) || Send(as,bs,m))
    case Send(as, b::bs, m) => nextChoreo(Send(as,List(b),m) || Send(as,bs,m))
    case Seq(c1, c2) =>
      val nc1 = nextChoreo(c1)
      val a1 = agents(c1)
      val nc2 = nextChoreo(c2)(ignore++a1)
      nc1.map(p=>p._1->simple(p._2>c2)) ++
        nc2.map(p=>p._1->simple(c1>p._2)) ++
        (if canSkip(c1) then nextChoreo(c2) else Nil)
    case Par(c1, c2) =>
      val nc1 = nextChoreo(c1)
      val nc2 = nextChoreo(c2)
      nc1.map(p => p._1 -> simple(p._2||c2)) ++
        nc2.map(p => p._1 -> simple(c1||p._2))
    case Choice(c1, c2) =>
      val nc1 = nextChoreo(c1)
      val nc2 = nextChoreo(c2)
      nc1 ++ nc2
    case Loop(c2) =>
      val nc2 = nextChoreo(c2)
      nc2.map(p=>p._1 -> (p._2>c))
    case End => Nil
    case Tau => Nil
    case In(a, b, m) =>
      if ignore contains a then Nil else List(In(a,b,m) -> End)
    case Out(a, b, m) =>
      if ignore contains a then Nil else List(Out(a,b,m) -> End)
    case _ => error("Unknonwn next for $c")

  def canSkip(c: Choreo): Boolean = c match
    case _:Send => false
    case Seq(c1, c2) => canSkip(c1) && canSkip(c2)
    case Par(c1, c2) => canSkip(c1) && canSkip(c2)
    case Choice(c1, c2) => canSkip(c1) || canSkip(c2)
    case Loop(_) => true
    case End => true
    case Tau => false 
    case _: Action => false
