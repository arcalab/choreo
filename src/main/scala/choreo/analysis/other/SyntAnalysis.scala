package choreo.analysis.other

import caos.common.Multiset
import choreo.analysis.Bounded._
import choreo.common.Simplify
import choreo.projection.ChorDefProj.proj
import choreo.sos.ChorDefSOS._
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

import scala.annotation.tailrec
import scala.sys.error

/**
 * This object combines 3 syntactic analysis:
 *   - An1: finding ?-sprints and check for incompatible choices
 *   - An2: find !-leaders and check for incompatible choices
 *   - An3: Experimental - find ?-guided choices and check for incompatibilities
 */
object SyntAnalysis:
  //todo: not updated to work with DChoice

  /////////////////////////////
  //// An1: Find ?-sprints ////
  /////////////////////////////

  ///// Action Multiset ////
  type AMultiset = Multiset[Action]

  /// Traces + evidences
  case class Trace(acts:AMultiset, c:Choreo, lookAhead:Option[Choreo]): // multiset + missing + next-Choreo
    override def toString: String =
      (if acts.isEmpty then "[]" else
        acts.toString) +
        " ~> " + c + (lookAhead match {
        case Some(nxt) => " ...> "+nxt
        case None => ""
      })
  private type Traces = Set[Trace]
  private type Evidence = (Trace,Trace)
  private type ToApprove = Map[AMultiset,Map[Option[Choreo],Evidence]] // could refactor code to drop Option

  /** Pretty print to show evidence of incompatibilities found */
  def ppTA(t:ToApprove): String = t.flatMap(me=> me._2.map(ev => "   - "+me._1+
    " BY "+ev._1+" otherwise incompatible: "+ev._2._1+" vs. "+ev._2._2)).mkString("\n")
  private type MbTraces = (Traces,ToApprove)

  /** Traverse a given Choreo's projection following all sequence of output actions until an input action is found. */
  def nextSprint(c:Choreo, a:Agent): MbTraces =
    nextSprint(proj(c,a))

  /** Traverse all projections of a Choreo expression, each by following all sequence of output actions until an input action is found. */
  def nextSprint(c:Choreo): MbTraces =
    nextSprintAux(Set(),Map(),Set(Trace(Multiset(),c,None)))

  /** Traverse a given Choreo's projection with [[nextSprint(c:Choreo,a:Agent)]] and produce a String with its report. */
  def nextSprintPP(c:Choreo,a:Agent): String = nextSprint(c, a) match
    //    case Left(ev) => s"Incompatible choice:\n - ${ev._1}\n - ${ev._2}"
    //    case Right((traces,pending)) =>
    case (traces,pending) =>
      traces.map(_.toString).mkString("\n") +
        (if pending.isEmpty then "" else
          "\n - Follow-up sprint(s) must exist:\n"+ppTA(pending))
  def allNextSprintPP(c:Choreo): String =
    (for a <- agents(c) yield s"--$a--\n${nextSprintPP(c,a)}").mkString("\n")


  @tailrec
  private def nextSprintAux(full: Traces, toApprove: ToApprove, partial: Traces) //  visited: History (=Choreo->Set[Act] for loops),
  : MbTraces =
    //println(s"//round started. full:${full.mkString(",")}\n  toApprove:$toApprove\n  partial:${partial.mkString(",")}")

    if partial.isEmpty then
      return (full, toApprove)

    var nFull: (Traces,ToApprove) = (full,Map())
    var nPartial: Traces = Set()

    for ptrace <- partial do
      val nxts = nextChoreo(ptrace.c)
      if accepting(ptrace.c) then // ptrace can (or must) stop: full trace found
        nFull = checkAndAdd(ptrace,nFull,None)
      for choice <- nxts do {
        //println(s"[NXT] next option: $choice")
        if choice._1.isOut then // ptrace can do a Out action - full trace found
          nFull = checkAndAdd(ptrace,nFull,Some(choice._2))
        else // ptrace found one more In action - add to partial trace
          val ntrace = addToTrace(choice,ptrace)
          nPartial = addToPartial(ntrace,nPartial)
      }
    //println(s"==round ended. full:${nFull._1.mkString(",")}\n  toApprove:${nFull._2}\n\\\\partial:${nPartial.mkString(",")}\n")
    nextSprintAux(nFull._1,joinToApprove(nFull._2,toApprove),nPartial)

  private def addToTrace(ac: (Action, Choreo), tr: Trace): Trace =
    if ac._1==Tau then
      Trace( tr.acts, Simplify(ac._2), tr.lookAhead)
    else // EXPERIMENT: not storing taus in traces   
      val res = Trace( tr.acts + ac._1, Simplify(ac._2), tr.lookAhead)
      //println(s"[ADDT] adding $ac to trace $tr --> $res")
      res

  //  private val badSend:Multiset = Map((""!"-or-End") -> 1)

  private def checkAndAdd(t: Trace, mbTraces: (Traces,ToApprove), nxt:Option[Choreo])
  : (Traces,ToApprove) =
    val trace = Trace(t.acts,t.c,nxt)
    mbTraces match
      //      case Left(_) => (mbTraces,oPend) // already failed
      case (traces,pending) =>
        // drop old pending
        var nPend2 = pending
        // check compatibility with existing traces - add to approve if incompatibility found
        for tr<-traces do
          if trace.acts != tr.acts then // optimization -- can go
            if trace.acts included tr.acts then
              val diff = tr.acts -- trace.acts
              //println(s"[ADD] incl1: $trace in $tr AND THEN ${trace.lookAhead} (diff $diff)")
              nPend2 += ( diff -> (nPend2.getOrElse(diff,Map()) + (trace.lookAhead -> (tr,trace))))
            else if tr.acts included trace.acts then
              val diff = trace.acts -- tr.acts
              //println(s"[ADD] incl2: $tr  INSIDE  $trace AND THEN ${tr.lookAhead} (diff $diff)")
              nPend2 += ( diff -> (nPend2.getOrElse(diff,Map()) + (tr.lookAhead    -> (trace,tr))))
        //          if ((trace.acts==tr.acts) && (trace.lookAhead.isDefined != tr.lookAhead.isDefined))
        //            nPend2 += ( badSend -> (nPend2.getOrElse(Map(),Map()) +
        //              (Some(trace.lookAhead.getOrElse(tr.lookAhead.get)) -> (tr,trace))))
        (traces + trace,nPend2)


  //  def included(t1: Trace, t2: Trace): Boolean =
  //    includedM(t1.acts,t2.acts)
  private def addToPartial(tr: Trace, partials: Traces): Traces =
    partials + tr

  private def joinToApprove(t1:ToApprove, t2:ToApprove): ToApprove =
  // t1: Multiset -> Opt[Cho] -> Evidence
    t1 ++ (for kv<-t2 yield kv._1 -> (kv._2 ++ t1.getOrElse(kv._1,Map())))

  /////////////////////////////////////////
  /////////////////////////////////////////

  /** Traverse a given Choreo's projection with [[nextSprint(c:Choreo,a:Agent)]] and produce evidences when problems are found. */
  def realisableIn(c:Choreo, a:Agent): Option[Evidence] =
    iterateNextSprintAg(List(proj(c,a)),Set())

  /** Traverse a given Choreo's projection with [[realisableIn(c:Choreo,a:Agent)]] and produce a readable report. */
  def realisableInPP(c:Choreo): String =
    val res = for a<-agents(c) yield
      (s"\n=== $a ===") +
        (realisableIn(c, a) match
          case Some(value) => s" - Not realisable. Evidence: \n     + ${value._1}\n     + ${value._2}"
          case None => " - OK - Could be realisable")
    res.mkString


  /** Traverse all projection of a Choreo expression with [[realisableIn(c:Choreo,a:Agent)]] and produce a readable report. */
  def realisableInPrint(c:Choreo): Unit =
    for a<-agents(c) do
      println(s"=== $a ===")
      realisableIn(c, a) match
        case Some(value) => println(s" - Not realisable. Evidence: \n     + ${value._1}\n     + ${value._2}")
        case None => println(" - OK - Could be realisable")

  @tailrec
  private def iterateNextSprintAg(next:List[Choreo], visited:Set[Choreo]): Option[Evidence] = next match
    case Nil => None
    case c::rest =>
      println(s"-- visiting $c --")
      val (traces,toApprove) = nextSprint(c)
      val res = reject(toApprove)
      res match
        case None =>
          val next2 = for (
            t<-traces if
            t.lookAhead.nonEmpty &&
              !visited.contains(t.lookAhead.get) &&
              !next.contains(t.lookAhead.get))
            yield t.lookAhead.get
          iterateNextSprintAg(rest++next2,visited+c)
        case Some(_) => res

  private def reject(approve: ToApprove): Option[Evidence] =
    var res: Option[Evidence] = None
    for ap <- approve; ext<-ap._2 do
      val (mset,cont,evid) = (ap._1,ext._1,ext._2)
      val (nxt,_) = nextSprint(cont.getOrElse(End))
      if !nxt.exists(t => mset included t.acts) then
        res = Some(evid)
      //// Debug:
      //      println(s"   [${if res.isDefined then "KO" else "OK"}] $mset"+
      //        s" BY $cont otherwise incompatible: ${evid._1}  VS.  ${evid._2}")
      // Without debug (stop at first evidence):
      if res.isDefined then return res
    res

  
  /////////////////////////
  // An2: choice-leaders //
  /////////////////////////

  type MbCLeader = Option[((Choreo,List[Choreo]),(Choreo,List[Choreo]))]

  /** Searches for realisability problems by checking if all choices have a clear !-leader. */
  def reaslisableOut(c:Choreo): MbCLeader = c match
    case Seq(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2)
    case Par(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2) orElse matchParalels(c1,c2)
    case Choice(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2) orElse matchLeaders(c1,c2)
    case Loop(c) => reaslisableOut(c)
    case End => None
    case Tau => None 
    case _: Action => None
    case _:Send => None
    case _ => error(s"Case $c:${c.getClass.getName} not supported.")

  private def matchLeaders(c1: Choreo, c2: Choreo): MbCLeader =
    val n1 = nextChoreo(c1).map(_._1).filter(_.isOut).toSet
    val n2 = nextChoreo(c2).map(_._1).filter(_.isOut).toSet
    val nn1 = n1 -- n2
    val nn2 = n2 -- n1
    (nn1.toList,nn2.toList) match
      case (Nil,Nil) => None
      case (List(Out(a1,_,_)),List(Out(a2,_,_))) if a1==a2 => None // one leader (and diff actions) - must be the same
      case (l1,l2) => Some((c1,l1),(c2,l2))

  private def matchParalels(c1: Choreo, c2: Choreo): MbCLeader =
    val shared = messages(c1) intersect messages(c2)
    if shared.isEmpty
    then None
    else Some((c1,shared.toList) , (c2,shared.toList))

  /** Searches for realisability problems by checking if all choices have a clear !-leader, and produces a readable report. */
  def realisableOutPP(c:Choreo): String = reaslisableOut(c) match
    case Some(((c1,l1),(c2,l2))) =>
      s"Failed choice:\n - '$c1' can do '${l1.mkString(",")}'\n - '$c2' can do '${l2.mkString(",")}'"
    case None => "OK - could be realisable"


  ///////////////////////////////////////////
  //// An3: experimenting with ?-leaders ////
  ///////////////////////////////////////////

  /** Searches for realisability problems by checking if all choices have a clear ?-leader. */
  def findInLeader(c:Choreo): MbCLeader = c match
    case Seq(c1, c2) => findInLeader(c1) orElse findInLeader(c2)
    case Par(c1, c2) => findInLeader(c1) orElse findInLeader(c2)
    case Choice(c1, c2) => findInLeader(c1) orElse findInLeader(c2) orElse matchInLeaders(c1,c2)
    case Loop(c) => findInLeader(c)
    case End => None
    case Tau => None 
    case _: Action => None
    case _:Send => None
    case _ => error(s"Case $c:${c.getClass.getName} not supported.")

  private def matchInLeaders(c1: Choreo, c2: Choreo): MbCLeader =
    val ags = agents(c1)++agents(c2)

    val n1 = (for ag<-ags yield
      nextChoreo(proj(c1,ag)).map(_._1).filter(!_.isOut).toSet).flatten
    val n2 = (for ag<-ags yield
      nextChoreo(proj(c2,ag)).map(_._1).filter(!_.isOut).toSet).flatten

    val nn1 = n1 -- n2
    val nn2 = n2 -- n1
    (nn1.toList,nn2.toList) match
      case (Nil,Nil) => None
      case (List(In(a1,_,_)),List(In(a2,_,_))) if a1==a2 => None // one leader (and diff actions) - must be the same
      case (l1,l2) => Some((c1,l1),(c2,l2))

  /** Searches for realisability problems by checking if all choices have a clear ?-leader, and produces a readable report. */
  def findInLeaderPP(c:Choreo): String = findInLeader(c) match
    case Some(((c1,l1),(c2,l2))) =>
      s"Failed choice:\n - '$c1' can do '${l1.mkString(",")}'\n - '$c2' can do '${l2.mkString(",")}'"
    case None => "OK - could be realisable"

  
  /////////////////////////////////////////////////////////////
  // Wrapping An1, An2, An3 (realisableIn and realisableOut) //
  /////////////////////////////////////////////////////////////

  /** Combines a set of experiments to detect syntactically potential problems for realisability. */
  def realisablePP(c:Choreo): String =
    (s"===== Expression =====\n$c")+
    (s"\n===== ! analysis =====\n${realisableOutPP(c)}")+
    (s"\n===== ? analysis (exp) =====\n${findInLeaderPP(c)}")+(
      if boundedChoreo(c) then
        (s"\n===== ? analysis (?-sprints) =====")+
        realisableInPP(c)
      else
        (s"\n===== Unbounded loop found - no ?-analysis =====")
    )

  /** Prints to the screen the result from a set of experiments to detect syntactically potential problems for realisability. */
  def realisablePrint(c:Choreo): Unit =
    println(realisablePP(c))
//    println(s"===== Expression =====\n$c")
//    println(s"===== ! analysis =====\n${realisableOutPP(c)}")
//    println(s"===== ? analysis (exp) =====\n${findInLeaderPP(c)}")
//    if boundedChoreo(c) then
//      println(s"===== ? analysis (?-sprints) =====")
//      realisableInPrint(c)
//    else
//      println(s"===== Unbounded loop found - no ?-analysis =====")

  /** Incomplete attempt to check realiability of Choreo expressions using syntactic analsysis. */
  def realisable(c:Choreo): Boolean =
    reaslisableOut(c).isEmpty &&
      findInLeader(c).isEmpty &&
      (if boundedChoreo(c) then
        agents(c).forall(realisableIn(c,_).isEmpty)
      else true)
      