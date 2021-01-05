package choreo.syntax

import choreo.syntax.Choreo2.ack

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.sys.error

case class Send(as:List[Agent2],bs:List[Agent2],m:Msg) extends Choreo2
case class Seq2(c1:Choreo2, c2:Choreo2)                extends Choreo2
case class Par2(c1:Choreo2, c2:Choreo2)                extends Choreo2
case class Choice2(c1:Choreo2, c2:Choreo2)             extends Choreo2
case class Loop2(c:Choreo2)                            extends Choreo2
case object End                                        extends Choreo2
// Action extends Choreo2

case class In(a:Agent2,b:Agent2,m:Msg)  extends Action
case class Out(a:Agent2,b:Agent2,m:Msg) extends Action

/**
 * Represent and analyse Choreo expressions.
 *
 * Usages:
 *  - start by importing: `import choreo.syntax.Choreo2._`
 *  - `a->"x" by m` - creates an expression from agent "a" to agent "x" with message "m"
 *  - `ex1`, ..., `ex3` - examples of choreo expressions
 *  - `go(ex2b)` - performs 1 step of ex2b and produces a pretty-print
 *  - `go(ex2b,3)` - performs 3 steps of ex2b and produces a pretty-print
 *  - `go(ex2b,99)` - performs all steps of ex2b (5 in this case) and produces a pretty-print
 *  - `proj(ex2b,a)` - projects ex2b into "a"
 *  - `allProj(ex2b)` - performs all projections for ex3, returning a map Agent->Choreo
 *  - `allProjPP(ex2b)` - same as before, but produces a pretty-print
 *  - `allNextSprintPP(ex2b)` - prints all possible ?-sprints for the next step (version without PP also exists)
 *  - `realisableInPP(ex8)` - prints the realisability ?-analysis for each agent in exp8
 *  - `realisableOutPP(ex8)` - prints the realisability !-analysis for each agent in exp8
 *  - `realisablePP(ex8)` - prints the realisability (? and !) analysis for each agent in exp8
 *
 */
object Choreo2:
  def loop(e:Choreo2): Loop2 = Loop2(e)
  val end: Choreo2 = End
  implicit def str2agent(s:String):Agent2 = Agent2(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))

  ////////////////////
  ///// Examples /////
  ////////////////////

  val a: Agent2 = Agent2("a")
  val b: Agent2 = Agent2("b")
  val c: Agent2 = Agent2("c")
  val d: Agent2 = Agent2("d")
  val x: Msg = Msg(List("x"))
  val y: Msg = Msg(List("y"))
  val m: Msg = Msg(List("m"))
  val ack: Msg = Msg(List("ack"))

  // 22 possible traces, size 6 (x1) or 8 (x21).
  val ex0: Choreo2 = (((a->b) + ((a->c) || (c->b))) > (b->d)) > (b->a) // not realsb: c can read or write
  // with pomsets it would be
  // a->b + (a->c || c->b) >  b->d  >  b->a  =
  //  1. a->b  >  b->d  >  b->a  (pomset with 6 nodes)
  //  2. (a->c || c->b) >  b->d  >  b->a   (pomset with 8 nodes)

  val exl1: Choreo2 = ((b→a)>(b→a)) + ((b→a)>(a→c))

  val ex1: Choreo2 = (b?"x1"|"m1") + (c?"x2"|"m1") > (b?"x3"|"m2") > (c?"x4"|"m2") // not realsb: b,c do not know if they can receive m2.
  val ex2a: Choreo2 = ((a→b)+(a→c)) > (c→d) // not realsb - c!d must wait for the decision of a, but may not know about it. (Also b waits or not.)
  val ex2b: Choreo2 = ((a→b)+(a→c)) > (d→c) // not realsb (b and c)
  val ex2c: Choreo2 = ((a→b)+(a→c)) > (a->c | m) // not realsb? c (and b is not termination aware)
  val ex2d: Choreo2 = ((a->b->c)+(a->c)) > (c->a | m) // realsb - NOT: b waits or not...
  val ex3: Choreo2 = (a?b + end) > a?c // not realisable (a may wait for b)
  val ex4: Choreo2 = ((a->b)+(a->c)) > (a->b|"end") > (a->c|"end") // realsb (if order preserving)
  val ex5: Choreo2 = (a->b->c + end) > (a->c|"end") > (a->b|"end") // not realsb (c waits for b?)
  val ex6: Choreo2 = (c->a) > (a->c || (b->a)) // incorrectly flagged as unrealisable... (fixed)
  val ex7: Choreo2 = a->c || (b->a) || (d->a) // may generate too many cases (unrealisable)
  val ex8: Choreo2 = a->c + (b->c) // not realsb (bad !-leader)
  val ex9a: Choreo2 = a->b > ((b->a|"go") + (b->a > (a->b|"stop"))) // dist1 - realisable
  val ex9b: Choreo2 = (a->b > (b->a|"go")) + (a->b > (b->a) > (a->b|"stop")) // dist2 - realisable
  val ex10: Choreo2 =  ((a->b|x)>(c->b|x)) + ((a->b|y)>(c->b|y)) // bad dependency between senders
  val ex10b: Choreo2 = ((a->b|x)>(c->b|y)) + ((a->b|y)>(c->b|y)) // OK
  val ex11: Choreo2 =  ((a->b->d)>(c->b|x)) + ((a->d->b)>(c->b|y)) // bad dependency between senders
  val ex12: Choreo2 =  ((a->b)>(c->d)) + ((a->d)>(c->b)) // bad dependency between senders
  val ex13: Choreo2 = ((a->c|"l1") > (b->c|"l2") > (a->b|"x") > (b->c|"l3"))  || // Tricky... Not realisable, but not captured (yet)
                      ((a->c|"r1") > (b->c|"r2") > (a->b|"x") > (b->c|"r3"))

  val g0: Choreo2 = end
  val g1: Choreo2 = end
  val g2: Choreo2 = end
  // 442675 possible traces (fast to compute)
  // Example from Emílio's (journal) paper - it feels unrealisable:
  //   c->a:quit/checkBalance/widthdraw can go even if "b->a:granted" did not go yet.
  val atm: Choreo2 =
    (c->a|"auth") > (a->b|"authReq") > (
      ((b->a|"denied") > (a->c|"authFailed")) + (
        (b->a|"granted") > (
          (c->a|"quit") + (
            (c->a|"checkBalance") > (
              (a->c|"advert") > g0 || (
                (a->c|"advert") > g1 || (
                  (b->a|"getBalance")  > g2
                )
              ) > (a->c|"balance")
            )
          ) /*quit+check*/ + (
            (c->a|"withdraw") > (a->b|"authWithdrawal") > (
              ((b->a|"allow") > (a->c|"money")) +
              ((b->a|"deny") > (a->c|"bye"))
            )
          )
        )
      )
    )
  val atm1: Choreo2 = next(atm).head._2
  val atm2: Choreo2 = next(atm1).head._2
  val atm3a: Choreo2 = next(atm2).head._2 // only this makes sense
  val atm3b: Choreo2 = next(atm2).apply(1)._2
  val atm3c: Choreo2 = next(atm2).apply(2)._2
  val atm3d: Choreo2 = next(atm2).apply(3)._2
  val atm4a: Choreo2 = next(atm3a).head._2 // only this makes sense
  val atm5a: Choreo2 = next(atm4a).head._2 // only these 2 make sense
  val atm5b: Choreo2 = next(atm4a).apply(1)._2 // only these 2 make sense
  val atm6ab: Choreo2 = next(atm5b).head._2 // only this makes sense

  val subatm: Choreo2 = (c->a|"quit") + ( (c->a|"check") > (b->a|"getBal"))

  val atmFromChorgram: Choreo2 = (c->a|"auth") >
    (a->b|"authReq") >
    (
      ((b->a|"denied") >
        (a->c|"authFail"))
        +
        (b->a|"granted") >
        (
          (c->a|"withdraw") >
            (a->b|"authWithdrawal") >
            (
              ((b->a|"allow") >
                (a->c|"money"))
                +
                ((b->a|"deny") >
                  (a->c|"bye"))
              )
              +
              ((c->a|"checkBalance") >
                (a->b|"getBalance") >
                (b->a|"balance") >
                (a->c|"balance"))
              +
              ((c->a|"quit") >
                (a->b|"quit"))
          )
      )


  ////////////////////////////////
  ////// SOS global semantics ////
  ////////////////////////////////

  def nextPP(c:Choreo2): String =
    val nc = next(c)(Set())
    nc.map(p=>s"${p._1} ~~> ${p._2}").mkString("\n")

  def nextSPP(c:Choreo2, n:Int): String =
    goS(c,n).mkString("\n")

  /** Older version, to be replaced by nextS. */
  private def goS(c:Choreo2,n:Int): List[String] = n match
    case 0 => List(s"~~> $c")
    case _ =>
      val nc = next(c)(Set())
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

  def nextS(c:Choreo2,n:Int): List[(List[Action],Choreo2)] = n match
    case 0 => List(Nil -> c)
    case _ =>
      val nc = next(c)(Set())
      nc.flatMap(p=> {
        val rec = nextS(p._2,n-1)
        if rec.isEmpty then
          List(List(p._1) -> End)
        else
          for s <- rec
            yield (p._1::s._1) -> s._2
      })

  // non-used experiments:
  def choicesG(c:Choreo2): List[Action] = next(c).map(_._1)
  def choicesL(c:Choreo2): String =
    allProj(c).view.mapValues(choicesG).mkString("\n")

  /** SOS: next step of a Choreo expression */
  def next(c:Choreo2)(implicit ignore:Set[Agent2]=Set()): List[(Action,Choreo2)] = c match
    case Send(List(a), List(b), m) =>
      if ignore contains a then Nil else List((a!b by m) -> (b?a by m))
    case Send(a::as, bs, m) => next(Send(List(a),bs,m) || Send(as,bs,m))
    case Send(as, b::bs, m) => next(Send(as,List(b),m) || Send(as,bs,m))
    case Seq2(c1, c2) =>
      val nc1 = next(c1)
      val a1 = agents(c1)
      val nc2 = next(c2)(ignore++a1)
      nc1.map(p=>p._1->simple(p._2>c2)) ++
      nc2.map(p=>p._1->simple(c1>p._2)) ++
        (if canSkip(c1) then next(c2) else Nil)
    case Par2(c1, c2) =>
      val nc1 = next(c1)
      val nc2 = next(c2)
      nc1.map(p => p._1 -> simple(p._2||c2)) ++
      nc2.map(p => p._1 -> simple(c1||p._2))
    case Choice2(c1, c2) =>
      val nc1 = next(c1)
      val nc2 = next(c2)
      nc1 ++ nc2
    case Loop2(c2) =>
      val nc2 = next(c2)
      nc2.map(p=>p._1 -> (p._2>c))
    case End => Nil
    case In(a, b, m) =>
      if ignore contains a then Nil else List(In(a,b,m) -> End)
    case Out(a, b, m) =>
      if ignore contains a then Nil else List(Out(a,b,m) -> End)
    case _ => error("Unknonwn next for $c")

  def canSkip(c: Choreo2): Boolean = c match
    case _:Send => false
    case Seq2(c1, c2) => canSkip(c1) && canSkip(c2)
    case Par2(c1, c2) => canSkip(c1) && canSkip(c2)
    case Choice2(c1, c2) => canSkip(c1) || canSkip(c2)
    case Loop2(_) => true
    case End => true
    case _: Action => false
  ///////////////////////
  ////// Utilities //////
  ///////////////////////

  /** Simple heuristic to apply some simplifications. */
  private def simpleOnce(c: Choreo2): Choreo2 = c match
    case Seq2(End, c2) => simpleOnce(c2)
    case Seq2(c1, End) => simpleOnce(c1)
    case Par2(End, c2) => simpleOnce(c2)
    case Par2(c1, End) => simpleOnce(c1)
    case Seq2(c1, c2) => simpleOnce(c1) >  simpleOnce(c2)
    case Par2(c1, c2) => simpleOnce(c1) || simpleOnce(c2)
    case Choice2(c1, c2) if c1==c2 => simpleOnce(c1)
    case Choice2(c1, c2) => simpleOnce(c1) + simpleOnce(c2)
    case Loop2(End) => End
    case Loop2(c2) => loop(simpleOnce(c2))
    case End | _:Send | _:Action => c
  @tailrec
  def simple(c: Choreo2): Choreo2 =
    val c2 = simpleOnce(c)
    if c2==c then c else simple(c2)

  def agents(c:Choreo2): Set[Agent2] = c match
    case Send(a, b, _) => a.toSet ++ b.toSet
    case Seq2(c1, c2) => agents(c1) ++ agents(c2)
    case Par2(c1, c2) => agents(c1) ++ agents(c2)
    case Choice2(c1, c2) => agents(c1) ++ agents(c2)
    case Loop2(c) => agents(c)
    case End => Set()
    case In(a, b, _)  => Set(a,b)
    case Out(a, b, _) => Set(a,b)

  /////////////////////////////////
  //// Projections into agents ////
  /////////////////////////////////

  /** Projects an expression into an agent. */
  def proj(c:Choreo2, a:Agent2): Choreo2 = simple(projAux(c,a))
  private def projAux(c:Choreo2, a:Agent2): Choreo2 = c match
    case Send(as, bs, m) =>
      val outs = as.filter(_==a).flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.filter(_==a).flatMap(b=>as.map(a2=>b?a2 by m))
      (outs++ins).fold(End)(_>_)
    case Seq2(c1, c2) => proj(c1,a) > proj(c2,a)
    case Par2(c1, c2) => proj(c1,a) || proj(c2,a)
    case Choice2(c1, c2) =>proj(c1,a) + proj(c2,a)
    case Loop2(c2) => loop(proj(c2,a))
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case End | _:In | _:Out => End

  def allProj(c:Choreo2): Map[Agent2,Choreo2] =
    (for a<-agents(c) yield a->proj(c,a))
      .toMap

  def allProjPP(c:Choreo2): String = allProj(c).mkString("\n")

  ////////////////////////
  //// Find ?-sprints ////
  ////////////////////////

  ///// Data types ////
  type Multiset = Map[Action,Int]
  def ppM(m:Multiset): String =
    (for e<-m yield (e._1.toString+",").repeat(e._2)).mkString("").dropRight(1)
  def mdiff(t1: Multiset, t2: Multiset): Multiset =
    (for at <- t1 if !t2.contains(at._1)
      yield at) ++ // all t1 that is not in t2
    (for at <- t1 if t2.contains(at._1) && t2(at._1)>at._2
      yield at._1->(at._2-t2(at._1))) // all t1 that is partially dropped by t2

  case class Trace(acts:Multiset,c:Choreo2,lookAhead:Option[Choreo2]): // multiset + missing + next-Choreo
    override def toString: String =
      (if acts.isEmpty then "[]" else
        ppM(acts)) +
        " ~> " + c + (lookAhead match {
        case Some(nxt) => " ...> "+nxt
        case None => ""
      })
  private type Traces = Set[Trace]
  private type Evidence = (Trace,Trace)
  private type ToApprove = Map[Multiset,Map[Option[Choreo2],Evidence]] // could refactor code to drop Option
  def ppTA(t:ToApprove): String = t.flatMap(me=> me._2.map(ev => "   - "+ppM(me._1)+
    " BY "+ev._1+" otherwise incompatible: "+ev._2._1+" vs. "+ev._2._2)).mkString("\n")
  private type MbTraces = (Traces,ToApprove)
//  private case class IncompatibleTraces(e: Evidence) extends RuntimeException

  //// traversal over agent projections ////
  def nextSprint(c:Choreo2, a:Agent2): MbTraces =
    nextSprint(proj(c,a))

  def nextSprint(c:Choreo2): MbTraces =
    nextSprintAux(Set(),Map(),Set(Trace(Map(),c,None)))

  def nextSprintPP(c:Choreo2,a:Agent2): String = nextSprint(c, a) match
//    case Left(ev) => s"Incompatible choice:\n - ${ev._1}\n - ${ev._2}"
//    case Right((traces,pending)) =>
    case (traces,pending) =>
      traces.map(_.toString).mkString("\n") +
        (if pending.isEmpty then "" else
          "\n - Follow-up sprint(s) must exist:\n"+ppTA(pending))
  def allNextSprintPP(c:Choreo2): String =
    (for a <- agents(c) yield s"--$a--\n${nextSprintPP(c,a)}").mkString("\n")


  @tailrec
  private def nextSprintAux(full: Traces, toApprove: ToApprove, partial: Traces) //  visited: History (=Choreo->Set[Act] for loops),
      : MbTraces =
//    println(s"//round started. full:${full.mkString(",")}\n  toApprove:$toApprove\n  partial:${partial.mkString(",")}")

    if partial.isEmpty then
      return (full, toApprove)

    var nFull: (Traces,ToApprove) = (full,Map())
    var nPartial: Traces = Set()

    for ptrace <- partial do
      val nxts = next(ptrace.c)
      if canSkip(ptrace.c) then // ptrace can (or must) stop: full trace found
        nFull = checkAndAdd(ptrace,nFull,None)
      for choice <- nxts do
        if choice._1.isOut then // ptrace can do a Out action - full trace found
          nFull = checkAndAdd(ptrace,nFull,Some(choice._2))
        else // ptrace found one more In action - add to partial trace
          val ntrace = addToTrace(choice,ptrace)
          nPartial = addToPartial(ntrace,nPartial)
//    println(s"==round ended. full:${nFull._1.mkString(",")}\n  toApprove:${nFull._2}\n\\\\partial:${nPartial.mkString(",")}\n")
    nextSprintAux(nFull._1,joinToApprove(nFull._2,toApprove),nPartial)

  private def addToTrace(ac: (Action, Choreo2), tr: Trace): Trace =
    val na:Int = tr.acts.getOrElse[Int](ac._1,0) + 1
    Trace(tr.acts + (ac._1 -> na) , simple(ac._2), tr.lookAhead)

//  private val badSend:Multiset = Map((""!"-or-End") -> 1)

  private def checkAndAdd(t: Trace, mbTraces: (Traces,ToApprove), nxt:Option[Choreo2])
       : (Traces,ToApprove) =
    val trace = Trace(t.acts,t.c,nxt)
    mbTraces match
//      case Left(_) => (mbTraces,oPend) // already failed
      case (traces,pending) =>
        // drop old pending
        var nPend2 = pending
        // check compatibility with existing traces - add to approve if incompatibility found
        for tr<-traces do
          if trace.acts.keys != tr.acts.keys then
            if included(trace,tr) then
              val diff = mdiff(tr.acts,trace.acts)
              //println(s"[ADD] incl1: $trace in $tr AND THEN ${trace.lookAhead} (diff $diff)")
              nPend2 += ( diff -> (nPend2.getOrElse(diff,Map()) + (trace.lookAhead -> (tr,trace))))
            else if included(tr,trace) then
              val diff = mdiff(trace.acts,tr.acts)
              //println(s"[ADD] incl2: $tr  INSIDE  $trace AND THEN ${tr.lookAhead} (diff $diff)")
              nPend2 += ( diff -> (nPend2.getOrElse(diff,Map()) + (tr.lookAhead    -> (trace,tr))))
//          if ((trace.acts==tr.acts) && (trace.lookAhead.isDefined != tr.lookAhead.isDefined))
//            nPend2 += ( badSend -> (nPend2.getOrElse(Map(),Map()) +
//              (Some(trace.lookAhead.getOrElse(tr.lookAhead.get)) -> (tr,trace))))
        (traces + trace,nPend2)


  def included(t1: Trace, t2: Trace): Boolean =
    included(t1.acts,t2.acts)

  def included(m1: Multiset, m2: Multiset): Boolean =
    m1.forall(a1 => m2.get(a1._1).exists(_>=a1._2))

  private def addToPartial(tr: Trace, partials: Traces): Traces =
    partials + tr

  private def joinToApprove(t1:ToApprove, t2:ToApprove): ToApprove =
    // t1: Multiset -> Opt[Cho] -> Evidence
    t1 ++ (for kv<-t2 yield kv._1 -> (kv._2 ++ t1.getOrElse(kv._1,Map())))

  def realisableIn(c:Choreo2, a:Agent2): Option[Evidence] =
    iterateNextSprintAg(List(proj(c,a)),Set())

  def realisableInPP(c:Choreo2): Unit =
    for a<-agents(c) do
      println(s"=== $a ===")
      realisableIn(c, a) match
        case Some(value) => println(s" - Not realisable. Evidence: \n     + ${value._1}\n     + ${value._2}")
        case None => println(" - Realisable")

  @tailrec
  def iterateNextSprintAg(next:List[Choreo2], visited:Set[Choreo2]): Option[Evidence] = next match
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

  def reject(approve: ToApprove): Option[Evidence] =
    var res: Option[Evidence] = None
    for ap <- approve; ext<-ap._2 do
      val (mset,cont,evid) = (ap._1,ext._1,ext._2)
//      if (mset == badSend)
//        res = Some(evid)
//      else {
        val (nxt,_) = nextSprint(cont.getOrElse(end))
//        if (!nxt.map(_.acts).contains(mset)) // replace by "some in next includes mset"
        if !nxt.exists(t => included(mset,t.acts)) then
          res = Some(evid)
//      }
      //// Debug:
      println(s"   [${if res.isDefined then "KO" else "OK"}] ${ppM(mset)}"+
        s" BY $cont otherwise incompatible: ${evid._1}  VS.  ${evid._2}")
      //// Without debug (stop at first evidence):
      // if (res.isDefined) return res
    res

  ////////////////////
  // choice-leaders //
  ////////////////////

  type MbCLeader = Option[((Choreo2,List[Action]),(Choreo2,List[Action]))]
  def reaslisableOut(c:Choreo2): MbCLeader = c match
    case Seq2(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2)
    case Par2(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2)
    case Choice2(c1, c2) => reaslisableOut(c1) orElse reaslisableOut(c2) orElse matchLeaders(c1,c2)
    case Loop2(c) => reaslisableOut(c)
    case End => None
    case _: Action => None
    case _:Send => None

  def matchLeaders(c1: Choreo2, c2: Choreo2): MbCLeader =
    val n1 = next(c1).map(_._1).filter(_.isOut).toSet
    val n2 = next(c2).map(_._1).filter(_.isOut).toSet
    val nn1 = n1 -- n2
    val nn2 = n2 -- n1
    (nn1.toList,nn2.toList) match
      case (Nil,Nil) => None
      case (List(Out(a1,_,_)),List(Out(a2,_,_))) if a1==a2 => None // one leader (and diff actions) - must be the same
      case (l1,l2) => Some((c1,l1),(c2,l2))

  def realisableOutPP(c:Choreo2): String = reaslisableOut(c) match
    case Some(((c1,l1),(c2,l2))) =>
      s"Failed choice:\n - '$c1' can do '${l1.mkString(",")}'\n - $c2 can do '${l2.mkString(",")}'"
    case None => "OK"

  def realisablePP(c:Choreo2): Unit =
    println(s"===== Expression =====\n$c")
    println(s"===== ! analysis =====\n${realisableOutPP(c)}")
    println(s"===== ? analysis =====")
    realisableInPP(c)

  /////////////////
  // freeChoices //
  /////////////////
  // non-working experiments

  def freeChoice(affected:Set[Agent2],c:Choreo2): (Set[Agent2],Set[Action]) = c match
    case Send(as, bs, m) =>
      ( if as.exists(ag => affected contains ag) then affected ++ bs else affected,
        for a:Agent2 <-as.toSet; b<-bs if !affected(a) yield Out(a,b,m) )
    case Seq2(c1, c2) =>
      val (af1,fc1) = freeChoice(affected,c1)
      val (af2,fc2) = freeChoice(af1,c2)
      (af2,fc1++fc2)
    case Par2(c1, c2) =>
      val (af1,fc1) = freeChoice(affected,c1)
      val (af2,fc2) = freeChoice(affected,c2)
      (af1++af2,fc1++fc2)
    case Choice2(c1, c2) =>
      val (af1,fc1) = freeChoice(affected,c1)
      val (af2,fc2) = freeChoice(affected,c2)
      (af1.intersect(af2),fc1++fc2)
    case Loop2(c) =>freeChoice(affected,c)
    case End => (affected,Set())
    case In(a, b, m) =>  if affected(b) then (affected+a,Set()) else (affected,Set())
    case Out(a, b, m) => (affected,Set())


  def proj2(ag:Agent2, affected:Set[Agent2],c:Choreo2): (Set[Agent2],Choreo2) =
    //println(s"proj2 - $ag - $affected - $c")
    c match
      //    case Send(as, bs, m) =>
      //      def seq(c1:Choreo2,c2:Choreo2): Choreo2 = Seq2(c1,c2)
      //      ( if (as.exists(ag => affected contains ag)) affected ++ bs else affected,
      //        (for (a2:Agent2 <-as.toSet; b<-bs) yield
      //          if (a2==ag) Out(a2,b,m)
      //          else if (b==ag) Out(a2,b,m+"0")>In(b,a2,m)
      //          else if (affected(a2)) End
      //          else Out(a2,b,m+"0")).fold[Choreo2](End)(seq))
      case Send(as,bs,m) =>
        val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
        proj2(ag,affected,cs.fold[Choreo2](end)(_>_))
      case Seq2(c1, c2) =>
        val (af1,fc1) = proj2(ag,affected,c1)
        val (af2,fc2) = proj2(ag,af1,c2)
        (af2,fc1>fc2)
      case Par2(c1, c2) =>
        val (af1,fc1) = proj2(ag,affected,c1)
        val (af2,fc2) = proj2(ag,affected,c2)
        (af1++af2,fc1||fc2)
      case Choice2(c1, c2) =>
        val (af1,fc1) = proj2(ag,affected,c1)
        val (af2,fc2) = proj2(ag,affected,c2)
        (af1.intersect(af2),fc1 + fc2)
      case Loop2(c2) =>
        val (af2,fc2) = proj2(ag,affected,c2)
        (af2,loop(fc2))
      case End => (affected,End)
      case In(a2, b, _) =>
        if a2==ag then (affected,c)
        else if affected(b) then
          (affected+a2,End)
        else (affected,End)
      case Out(a2, _, _) =>
        if a2==ag then (affected,c)
        else if affected(a2) then
          (affected,End)
        else (affected,c|"0")

  def proj2(ag:Agent2,c:Choreo2): (Set[Agent2],Choreo2) =
    val (a2,b) = proj2(ag,Set(ag),c)
    (a2,simple(b))


  /////////////////////////
  /// Luc's experiments ///
  /////////////////////////

  def compat(c1:Choreo2,c2:Choreo2): Boolean =
    c1==c2 || distp(c1,c2)

  def dista(c1:Choreo2,c2:Choreo2): Boolean = (c1,c2) match
    case (Send(as, bs, m),_) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      dista(cs.fold[Choreo2](end)(_>_),c2)
    case (_,Send(as, bs, m)) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      dista(c2,cs.fold[Choreo2](end)(_>_))

    case (Seq2(c1a, c1b),Seq2(c2a,c2b)) if c1a==c2a => dista(c1b,c2b)
    case (Seq2(c1a,_),Seq2(c2a,_)) => dista(c1a,c2a)
    case (Seq2(c1a,_),_) => dista(c1a,c2)
    case (_,Seq2(c2a,_)) => dista(c1,c2a)
    case (Par2(_,_),_) => error("[dista] parallel not handled")
    case (_,Par2(_,_)) => error("[dista] parallel not handled")
    case (Choice2(c1a, c1b),_) => dista(c1a,c2) || dista(c1b,c2)
    case (_,Choice2(c2a, c2b)) => dista(c1,c2a) || dista(c1,c2b)
    case (Loop2(_),_) => error("[dista] loops nos handled")
    case (_,Loop2(_)) => error("[dista] loops nos handled")
    case (Out(a,_,_),Out(b,_,_)) => a==b
    case (Out(_,_,_),_) => false
    case (_,Out(_,_,_)) => false
    case (In(_,_,_),_) => false
    case (_,In(_,_,_)) => false
    case (End,End) => false
    case (End,_) => true // ?
    case (_,End) => true // ?

  def distp(c1:Choreo2, c2:Choreo2): Boolean = (c1,c2) match
    case (Send(as, bs, m),_) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      distp(cs.fold[Choreo2](end)(_>_),c2)
    case (_,Send(as, bs, m)) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      distp(c2,cs.fold[Choreo2](end)(_>_))

    case (Seq2(c1a, c1b),Seq2(c2a,c2b)) if c1a==c2a => distp(c1b,c2b)
    case (Seq2(c1a,_),Seq2(c2a,_)) => distp(c1a,c2a)
    case (Seq2(c1a,_),_) => distp(c1a,c2)
    case (_,Seq2(c2a,_)) => distp(c1,c2a)
    case (Par2(_,_),_) => error("[dista] parallel not handled")
    case (_,Par2(_,_)) => error("[dista] parallel not handled")
    case (Choice2(c1a, c1b),_) => distp(c1a,c2) || distp(c1b,c2)
    case (_,Choice2(c2a, c2b)) => distp(c1,c2a) || distp(c1,c2b)
    case (Loop2(_),_) => error("[dista] loops nos handled")
    case (_,Loop2(_)) => error("[dista] loops nos handled")
    case (End,End) => false
    case (End,_) => true // ?
    case (_,End) => true // ?
    case (In(a1,b1,m1),In(a2,b2,m2)) => (a1==a2) && (b1!=b2) && (m1!=m2)
    case (In(_,_,_),_) => false
    case (_,In(_,_,_)) => false
    case (Out(_,_,_),_) => false
    case (_,Out(_,_,_)) => false

  def compat(ag:Agent2, c1:Choreo2, c2:Choreo2): Boolean =
    compat(proj(c1,ag),proj(c2,ag))
  def dista(ag:Agent2, c1:Choreo2, c2:Choreo2): Boolean =
    dista(proj(c1,ag),proj(c2,ag))
  def distp(ag:Agent2, c1:Choreo2, c2:Choreo2): Boolean =
    distp(proj(c1,ag),proj(c2,ag))


  def realisableLuc(c:Choreo2): Boolean = c match
    case Send(_, _, _) => true
    case Seq2(c1, c2) =>
      val r1 = realisableLuc(c1)
      val r2 = realisableLuc(c2)
      println(s"[real] '$c1'($r1) ; '$c2'($r2)")
      r1 && r2
    case Par2(c1, c2) => error("[real] parallel not handled")
    case Choice2(c1, c2) =>
      val r1 = realisableLuc(c1)
      val r2 = realisableLuc(c2)
      val ags = agents(c1)++agents(c2)
      val r3 = ags.forall(a => compat(a,c1,c2))
      val r4 = ags.exists(b => dista(b,c1,c2) &&
                               (ags-b).forall(a => compat(a,c1,c2)))
      println(s"[real] '$c1'($r1) +[$r3,$r4] '$c2'($r2)")
      r1 && r2 &&  (r3 || r4)
    case Loop2(c) => error("[real] loops not handled")
    case End => true
    case _: Action => true

  /////////////////////////
  //// Extract choices ////
  /////////////////////////
  // (a+b)*  =  ?? 0  |||  0 + a;(a+b)* + b;(a+b)*  |||
  //                  0 + a;(a+b)* + b;(a+b)* + a;a;(a+b)* + a;b;(a+b)* + b;a;(a+b)* + b;b;(a+b)* ...
  // (a+b) ; c  = a;c + b;c
  // (a+b) || c = a||c + b||c


  ////////////////////////////////
  //// Analyse realisability? ////
  ////////////////////////////////

  /*
  def findReal(c:Choreo2): Option[...]
    - Overall idea: start with Set(c); pop

    - start with all "step->c2"
    - collect all steps that are !
      - if from different agents -> not realisable
      - if from the same agent "a"
        - collect all other steps (?)
           - if any is from the same agent -> not realisable
           -
        - store set of destinations c2
        - if set was known -> realisable!
   */

sealed abstract class Action  extends Choreo2:
  override def by(m:Msg): Action = this match
    case In(a, b, m2) => In(a,b,m++m2)
    case Out(a, b, m2) => Out(a,b,m++m2)
  def isOut: Boolean = this match
    case _:Out => true
    case _ => false

sealed trait Choreo2:
  def >(e:Choreo2): Choreo2 = Seq2(this,e)
  def ||(e:Choreo2): Choreo2 = Par2(this,e)
  def +(e:Choreo2): Choreo2 = Choice2(this,e)
  def ->(a:Agent2): Choreo2 = this > Send(lastActs,List(a),Msg(Nil))
  def -->(a:Agent2): Choreo2 = this > Send(lastActs,List(a),Msg(Nil)) > Send(List(a),lastActs,ack)
  def loop: Choreo2 = Loop2(this)
  def by(m:Msg):Choreo2 = this match
    case Send(a, b, m2) => Send(a,b,m++m2)
    case Seq2(c1, c2) => Seq2(c1 by m,c2 by m)
    case Par2(c1, c2) => Par2(c1 by m,c2 by m)
    case Choice2(c1, c2) => Choice2(c1 by m,c2 by m)
    case Loop2(c) => Loop2(c by m)
    case End => End
    case _:Action => error("`by` is overriden in Action")
  def |(m:Msg):Choreo2 = by(m)
  def ::(m:Msg):Choreo2 = by(m)

  @tailrec
  private def lastActs: List[Agent2] = this match
    case Send(_, bs, _) => bs
    case Seq2(c1, Send(_,_,Msg("ack"::_))) => c1.lastActs
    case Seq2(_, c2) => c2.lastActs
    case Out(_,b,_) => List(b)
    case _ => error(s"No last action found to connect $this")

  override def toString: String = this match
    case Send(a, b, m) => s"${a.mkString(",")}->${b.mkString(",")}${m.pp}"
    case In(a,b,m)  => s"$a?$b${m.pp}"
    case Out(a,b,m) => s"$a!$b${m.pp}"
    case Seq2(c1, c2) =>s"${mbP(c1)} ; ${mbP(c2)}"
    case Par2(c1, c2) =>s"${mbP(c1)} || ${mbP(c2)}"
    case Choice2(c1, c2) => s"${mbP(c1)} + ${mbP(c2)}"
    case Loop2(c) => s"${mbP(c)}^*"
    case End => "0"

  private def mbP(choreo: Choreo2): String = choreo match
    case _:Seq2| _:Par2 | _:Choice2 => s"($choreo)"
    case _ => choreo.toString

//case class Tag(c:Choreo2,m:Msg) extends Choreo2


case class Msg(l:List[String]):
  def pp:String = if l.isEmpty then "" else ":"+l.reverse.mkString("/")
  def +(m:String): Msg = Msg(m::l)
  def ++(m:Msg): Msg = Msg(m.l:::l)

case class Agent2(s:String):
  def !(to:Agent2): Out = Out(this,to,Msg(Nil))
  def ?(from:Agent2): In = In(this,from,Msg(Nil))

  def ->(to:Agent2): Send = Send(List(this),List(to),Msg(Nil))
  def →(to:Agent2): Send = ->(to)
  def -->(b:Agent2): Choreo2 = (this->b) > ((b->this) by ack)

  override def toString: String = s

