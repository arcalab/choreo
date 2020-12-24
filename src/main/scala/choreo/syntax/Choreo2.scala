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

case class In(a:Agent2,b:Agent2,m:Msg)  extends Action(a,b,m)
case class Out(a:Agent2,b:Agent2,m:Msg) extends Action(a,b,m)

/**
 * Represent and analyse Choreo expressions.
 *
 * Usages:
 *  - start by importing: `import choreo.syntax.Choreo2._`
 *  - `a->"x" by m` - creates an expression from agent "a" to agent "x" with message "m"
 *  - `ex1`, ..., `ex3` - examples of choreo expressions
 *  - `go(ex3)` - performs 1 step of ex3 and produces a pretty-print
 *  - `go(ex3,3)` - performs 3 steps of ex3 and produces a pretty-print
 *  - `go(ex3,99)` - performs all steps of ex3 (5 in this case) and produces a pretty-print
 *  - `proj(ex3,a)` - projects ex3 into "a"
 *  - `allProj(ex3)` - performs all projections for ex3, returning a map Agent->Choreo
 *  - `allProjPP(ex3)` - same as before, but produces a pretty-print
 *
 */
object Choreo2 {
  def loop(e:Choreo2): Loop2 = Loop2(e)
  implicit def str2agent(s:String):Agent2 = Agent2(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))

  ////////////////////
  ///// Examples /////
  ////////////////////

  val a: Agent2 = Agent2("a")
  val b: Agent2 = Agent2("b")
  val c: Agent2 = Agent2("c")
  val d: Agent2 = Agent2("d")
  val m: Msg = Msg(List("m"))
  val n: Msg = Msg(List("n"))
  val ack: Msg = Msg(List("ack"))

  // 22 possible traces, size 6 (x1) or 8 (x21).
  val ex1: Choreo2 = (((a->b) + ((a->c) || (c->b))) > (b->d)) > (b->a) // not realsb: c can read or write
  // with pomsets it would be
  // a->b + (a->c || c->b) >  b->d  >  b->a  =
  //  1. a->b  >  b->d  >  b->a  (pomset with 6 nodes)
  //  2. (a->c || c->b) >  b->d  >  b->a   (pomset with 8 nodes)

  val ex2: Choreo2 = (b?"x1" by "m1") + (c?"x2" by "m1") > (b?"x3" by "m2") > (c?"x4" by "m2") // not realsb: b,c do not know if they can receive m2.
  val ex3: Choreo2 = ((a→b)+(a→c)) > (c→d) // not realsb - c!d must wait for the decision of a, but may not know about it. (Also b waits or not.)
  val ex4: Choreo2 = ((a→b)+(a→c)) > (d→c) // not realsb (b and c)
  val ex5: Choreo2 = ((a→b)+(a→c)) > (a→c by m) // not realsb? c (and b is not termination aware)
  val ex6: Choreo2 = ((a->b->c)+(a→c)) > (c→a by m) // realsb - NOT: b waits or not...
  val ex7: Choreo2 = ((a->b)+(a->c)) > (a->b by "end") > (a->c by "end") // realsb (if order preserving)
  val ex8: Choreo2 = (a->b->c + End) > (a->c by "end") > (a->b by "end") // not realsb (c waits for b?)
  val ex9: Choreo2 = (c->a) > (a->c || (b->a)) // incorrectly flagged as unrealisable...

  val g0: Choreo2 = End
  val g1: Choreo2 = End
  val g2: Choreo2 = End
  // 442675 possible traces (fast to compute)
  // Example from Emílio's (journal) paper - it feels unrealisable:
  //   c->a:quit/checkBalance/widthdraw can go even if "b->a:granted" did not go yet.
  val atm: Choreo2 =
    (c->a by "auth") > (a->b by "authReq") > (
      ((b->a by "denied") > (a->c by "authFailed")) + (
        (b->a by "granted") > (
          (c->a by "quit") + (
            (c->a by "checkBalance") > (
              (a->c by "advert") > g0 || (
                (a->c by "advert") > g1 || (
                  (b->a by "getBalance")  > g2
                )
              ) > (a->c by "balance")
            )
          ) /*quit+check*/ + (
            (c->a by "withdraw") > (a->b by "authWithdrawal") > (
              ((b->a by "allow") > (a->c by "money")) +
              ((b->a by "deny") > (a->c by "bye"))
            )
          )
        )
      )
    )
  val atm1  = next(atm).head._2
  val atm2  = next(atm1).head._2
  val atm3a = next(atm2).head._2 // only this makes sense
  val atm3b = next(atm2).apply(1)._2
  val atm3c = next(atm2).apply(2)._2
  val atm3d = next(atm2).apply(3)._2
  val atm4a = next(atm3a).head._2 // only this makes sense
  val atm5a = next(atm4a).head._2 // only these 2 make sense
  val atm5b = next(atm4a).apply(1)._2 // only these 2 make sense
  val atm6ab = next(atm5b).head._2 // only this makes sense


  ////////////////////////////////
  ////// SOS global semantics ////
  ////////////////////////////////

  def go(c:Choreo2): String = {
    val nc = next(c)(Set())
    nc.map(p=>s"${p._1} ~~> ${p._2}").mkString("\n")
  }

  def go(c:Choreo2,n:Int): String =
    goS(c,n).mkString("\n")

  /** Older version, to be replaced by nextS. */
  private def goS(c:Choreo2,n:Int): List[String] = n match {
    case 0 => List(s"~~> $c")
    case _ =>
      val nc = next(c)(Set())
      nc.flatMap(p=> {
        val rec = goS(p._2,n-1)
        if (rec.isEmpty)
          List(s"${p._1} [Done]")
        else {
          var fst = true
          val indent = " ".repeat(p._1.toString.length)+"   "
          for (s <- rec)
            yield s"${if (fst) {fst=false;p._1+" \\ "} else indent}$s"
        }
      })
  }

  def nextS(c:Choreo2,n:Int): List[(List[Action],Choreo2)] = n match {
    case 0 => List(Nil -> c)
    case _ =>
      val nc = next(c)(Set())
      nc.flatMap(p=> {
        val rec = nextS(p._2,n-1)
        if (rec.isEmpty)
          List(List(p._1) -> End)
        else
          for (s <- rec)
            yield (p._1::s._1) -> s._2
      })
  }

  def choicesG(c:Choreo2): List[Action] = next(c).map(_._1)
  def choicesL(c:Choreo2): String =
    allProj(c).mapValues(choicesG).mkString("\n")

  /** SOS: next step of a Choreo expression */
  def next(c:Choreo2)(implicit ignore:Set[Agent2]=Set()): List[(Action,Choreo2)] = c match {
    case Send(List(a), List(b), m) =>
      if (ignore contains a) Nil else List((a!b by m) -> (b?a by m))
    case Send(a::as, bs, m) => next(Send(List(a),bs,m) || Send(as,bs,m))
    case Send(as, b::bs, m) => next(Send(as,List(b),m) || Send(as,bs,m))
    case Seq2(c1, c2) =>
      val nc1 = next(c1)
      val a1 = agents(c1)
      val nc2 = next(c2)(ignore++a1)
      nc1.map(p=>p._1->simple(p._2>c2)) ++
      nc2.map(p=>p._1->simple(c1>p._2)) ++
        (if (canSkip(c1)) next(c2) else Nil)
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
      if (ignore contains a) Nil else List(In(a,b,m) -> End)
    case Out(a, b, m) =>
      if (ignore contains a) Nil else List(Out(a,b,m) -> End)
    case _ => error("Unknonwn next for $c")
  }

  def canSkip(c: Choreo2): Boolean = c match {
    case _:Send => false
    case Seq2(c1, c2) => canSkip(c1) && canSkip(c2)
    case Par2(c1, c2) => canSkip(c1) && canSkip(c2)
    case Choice2(c1, c2) => canSkip(c1) || canSkip(c2)
    case Loop2(_) => true
    case End => true
    case _: Action => false
  }
  ///////////////////////
  ////// Utilities //////
  ///////////////////////

  /** Simple heuristic to apply some simplifications. */
  private def simpleOnce(c: Choreo2): Choreo2 = c match {
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
  }
  @tailrec
  def simple(c: Choreo2): Choreo2 = {
    val c2 = simpleOnce(c)
    if (c2==c) c else simple(c2)
  }

  def agents(c:Choreo2): Set[Agent2] = c match {
    case Send(a, b, _) => a.toSet ++ b.toSet
    case Seq2(c1, c2) => agents(c1) ++ agents(c2)
    case Par2(c1, c2) => agents(c1) ++ agents(c2)
    case Choice2(c1, c2) => agents(c1) ++ agents(c2)
    case Loop2(c) => agents(c)
    case End => Set()
    case In(a, b, _)  => Set(a,b)
    case Out(a, b, _) => Set(a,b)
  }

  /////////////////////////////////
  //// Projections into agents ////
  /////////////////////////////////

  def proj(c:Choreo2, a:Agent2): Choreo2 = c match {
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
  }

  def allProj(c:Choreo2): Map[Agent2,Choreo2] =
    (for (a<-agents(c)) yield a->simple(proj(c,a)))
      .toMap

  def allProjPP(c:Choreo2): String = allProj(c).mkString("\n")

  ////////////////////////
  //// Find ?-sprints ////
  ////////////////////////

  case class Trace(acts:Map[Action,Int],last:Action,c:Choreo2) { // multiset + last action + next-Choreo
    override def toString: String =
      if (acts.isEmpty) "~> "+c else
        (for (a<-(acts-last)) yield (a._1.toString+",").repeat(a._2))
          .mkString("") +
          last +
          " ~> " + c
  }
  private type Traces = Set[Trace]
  private type Evidence = (Trace,Trace)
  private type MbTraces = Either[Evidence,Traces]
  private case class IncompatibleTraces(e: Evidence) extends RuntimeException

  def nextSprint(c:Choreo2, a:Agent2) =
    nextSprintAux(Set(),Set(Trace(Map(),In(a,b,Msg(Nil)),simple(proj(c,a)))))
      //.right.get // for now...

  def nextSprintPP(c:Choreo2,a:Agent2) = nextSprint(c, a) match {
    case Left(ev) => s"Incompatible choice:\n - ${ev._1}\n - ${ev._2}"
    case Right(value) => value.mkString("\n")
  }
  def allNextSprintPP(c:Choreo2) =
    (for (a <- agents(c)) yield s"--$a--\n${nextSprintPP(c,a)}").mkString("\n")


  @tailrec
  private def nextSprintAux(full: Traces, partial: Traces)
      : MbTraces = {
    if (partial.isEmpty) return Right(full)
    var nFull: Either[Evidence,Traces] = Right(full)
    var nPartial: Traces = Set()
    for (ptrace <- partial) {
      val nxts = next(ptrace.c)
//      if (nxts.isEmpty) // nowhwere to go from ptrace: full trace found
//        nFull = checkAndAdd(ptrace,nFull) // will also check if it can add
//      else {
      if (canSkip(ptrace.c)) // ptrace can (or must) stop: full trace found
        nFull = checkAndAdd(ptrace,nFull)
      for (choice <- nxts) {
        if (choice._1.isOut) // ptrace can do a Out action - full trace found
          nFull = checkAndAdd(ptrace,nFull)
        else { // ptrace found one more In action - add to partial trace
          val ntrace = addToTrace(choice,ptrace)
          nPartial = addToPartial(ntrace,nPartial)
        }
      }
//      }
    }
    nFull match {
      case Left(_) => nFull
      case Right(value) => nextSprintAux(value,nPartial)
    }
  }

  private def addToTrace(ac: (Action, Choreo2), tr: Trace): Trace = {
    val na:Int = tr.acts.getOrElse[Int](ac._1,0) + 1
    Trace(tr.acts + (ac._1 -> na) , ac._1 , simple(ac._2))
  }

  private def checkAndAdd(trace: Trace, mbTraces: MbTraces): MbTraces = {
    mbTraces match {
      case Left(_) => mbTraces
      case Right(traces) =>
        for (tr<-traces)
          if (trace.acts.keys != tr.acts.keys &&
              (included(trace,tr) || included(tr,trace)))
            return Left((tr,trace))
        Right(traces + trace)
    }
  }

  private def included(t1: Trace, t2: Trace): Boolean = {
    // need to partition in traces from the same agent (?)
    t1.acts.forall(a1 => t2.acts.get(a1._1).exists(_>=a1._2))
  }

  private def addToPartial(tr: Trace, partials: Traces): Traces =
    partials + tr

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
}

sealed abstract class Action(a:Agent2,b:Agent2,m:Msg)  extends Choreo2 {
  override def by(m:Msg): Action = this match {
    case In(a, b, m2) => In(a,b,m++m2)
    case Out(a, b, m2) => Out(a,b,m++m2)
  }
  def isOut: Boolean = this match {
    case _:Out => true
    case _ => false
  }
}

sealed trait Choreo2 {
  def >(e:Choreo2): Choreo2 = Seq2(this,e)
  def ||(e:Choreo2): Choreo2 = Par2(this,e)
  def +(e:Choreo2): Choreo2 = Choice2(this,e)
  def ->(a:Agent2): Choreo2 = this > Send(lastActs,List(a),Msg(Nil))
  def -->(a:Agent2): Choreo2 = this > Send(lastActs,List(a),Msg(Nil)) > Send(List(a),lastActs,ack)
  def loop: Choreo2 = Loop2(this)
  def by(m:Msg):Choreo2 = this match {
    case Send(a, b, m2) => Send(a,b,m++m2)
    case Seq2(c1, c2) => Seq2(c1 by m,c2 by m)
    case Par2(c1, c2) => Par2(c1 by m,c2 by m)
    case Choice2(c1, c2) => Choice2(c1 by m,c2 by m)
    case Loop2(c) => Loop2(c by m)
    case End => End
    case _:Action => error("`by` is overriden in Action")
  }

  @tailrec
  private def lastActs: List[Agent2] = this match {
    case Send(_, bs, _) => bs
    case Seq2(c1, Send(_,_,Msg("ack"::_))) => c1.lastActs
    case Seq2(_, c2) => c2.lastActs
    case Out(_,b,_) => List(b)
    case _ => error(s"No last action found to connect $this")
  }

  override def toString: String = this match {
    case Send(a, b, m) => s"${a.mkString(",")}->${b.mkString(",")}${m.pp}"
    case In(a,b,m)  => s"$a?$b${m.pp}"
    case Out(a,b,m) => s"$a!$b${m.pp}"
    case Seq2(c1, c2) =>s"${mbP(c1)} ; ${mbP(c2)}"
    case Par2(c1, c2) =>s"${mbP(c1)} || ${mbP(c2)}"
    case Choice2(c1, c2) => s"${mbP(c1)} + ${mbP(c2)}"
    case Loop2(c) => s"${mbP(c)}^*"
    case End => "0"
  }

  private def mbP(choreo: Choreo2): String = choreo match {
    case _:Seq2| _:Par2 | _:Choice2 => s"($choreo)"
    case _ => choreo.toString
  }
}

//case class Tag(c:Choreo2,m:Msg) extends Choreo2


case class Msg(l:List[String]) {
  def pp:String = if (l.isEmpty) "" else ":"+l.reverse.mkString("/")
  def +(m:String): Msg = Msg(m::l)
  def ++(m:Msg): Msg = Msg(m.l:::l)
}

case class Agent2(s:String) {
  def !(to:Agent2): Out = Out(this,to,Msg(Nil))
  def ?(from:Agent2): In = In(this,from,Msg(Nil))

  def ->(to:Agent2): Send = Send(List(this),List(to),Msg(Nil))
  def →(to:Agent2): Send = ->(to)
  def -->(b:Agent2): Choreo2 = (this->b) > ((b->this) by ack)

  override def toString: String = s
}

