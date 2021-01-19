package choreo.choreo2.syntax

import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.syntax.Msg._
import choreo.choreo2.syntax.{Agent, Choreo, Msg}
import choreo.choreo2.backend.Simplify._

import scala.annotation.tailrec
import scala.sys.error

sealed trait Choreo:
  def >(e:Choreo): Choreo = Seq(this,e)
  def ||(e:Choreo): Choreo = Par(this,e)
  def +(e:Choreo): Choreo = Choice(this,e)
  def ->(a:Agent): Choreo = this > Send(lastActs,List(a),Msg(Nil))
  def -->(a:Agent): Choreo = this > Send(lastActs,List(a),Msg(Nil)) > Send(List(a),lastActs,ack)
  def loop: Choreo = Loop(this)
  def by(m:Msg):Choreo = this match
    case Send(a, b, m2) => Send(a,b,m++m2)
    case Seq(c1, c2) => Seq(c1 by m,c2 by m)
    case Par(c1, c2) => Par(c1 by m,c2 by m)
    case Choice(c1, c2) => Choice(c1 by m,c2 by m)
    case Loop(c) => Loop(c by m)
    case End => End
    case Tau => Tau
    case In(a, b, m2) => In(a,b,m++m2)
    case Out(a, b, m2) => Out(a,b,m++m2)
  def |(m:Msg):Choreo = by(m)
  def ::(m:Msg):Choreo = by(m)

  @tailrec
  private def lastActs: List[Agent] = this match
    case Send(_, bs, _) => bs
    case Seq(c1, Send(_,_,Msg("ack"::_))) => c1.lastActs
    case Seq(_, c2) => c2.lastActs
    case Out(_,b,_) => List(b)
    case _ => error(s"No last action found to connect $this")

  override def toString: String = this match
    case Send(a, b, m) => s"${a.mkString(",")}->${b.mkString(",")}${m.pp}"
    case In(a,b,m)  => s"$a?$b${m.pp}"
    case Out(a,b,m) => s"$a!$b${m.pp}"
    case Tau => "τ"
    case Seq(c1, c2) =>s"${mbP(c1)} ; ${mbP(c2)}"
    case Par(c1, c2) =>s"${mbP(c1)} || ${mbP(c2)}"
    case Choice(c1, c2) => s"${mbP(c1)} + ${mbP(c2)}"
    case Loop(c) => s"${mbP(c)}^*"
    case End => "0"

  private def mbP(choreo: Choreo): String = choreo match
    case _:Seq| _:Par | _:Choice => s"($choreo)"
    case _ => choreo.toString

    //case class Tag(c:Choreo,m:Msg) extends Choreo

object Choreo:
  case class Send(as:List[Agent],bs:List[Agent],m:Msg)  extends Choreo
  case class Seq(c1:Choreo, c2:Choreo)                  extends Choreo
  case class Par(c1:Choreo, c2:Choreo)                  extends Choreo
  case class Choice(c1:Choreo, c2:Choreo)               extends Choreo
  case class Loop(c:Choreo)                             extends Choreo
  case object End                                       extends Choreo
  
  // Action extends Choreo
  sealed abstract class Action  extends Choreo:
    def isOut: Boolean = this match
      case _:Out => true
      case _ => false
  
  case class In(a:Agent,b:Agent,m:Msg)  extends Action
  case class Out(a:Agent,b:Agent,m:Msg) extends Action
  case object Tau                       extends Action

  ///////////////////////////////
  //// Projections into agents ////
  /////////////////////////////////

  /** Projects an expression into an agent. */
  def proj(c:Choreo, a:Agent): Choreo = simple(projAux(c,a))
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      //      val outs = as.filter(_==a).flatMap(a2=>bs.map(b=>a2!b by m))
      //      val ins  = bs.filter(_==a).flatMap(b=>as.map(a2=>b?a2 by m))
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      proj((outs++ins).fold(End)(_>_) , a)
    case Seq(c1, c2) => proj(c1,a) > proj(c2,a)
    case Par(c1, c2) => proj(c1,a) || proj(c2,a)
    case Choice(c1, c2) =>proj(c1,a) + proj(c2,a)
    case Loop(c2) => Loop(proj(c2,a))
    case End => End
    case Tau => Tau 
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case _:In | _:Out => End //Tau

  def allProj(c:Choreo): Map[Agent,Choreo] =
    (for a<-agents(c) yield a->proj(c,a))
      .toMap

  def allProjPP(c:Choreo): String = allProj(c).mkString("\n")

  def agents(c:Choreo): Set[Agent] = c match
    case Send(a, b, _) => a.toSet ++ b.toSet
    case Seq(c1, c2) => agents(c1) ++ agents(c2)
    case Par(c1, c2) => agents(c1) ++ agents(c2)
    case Choice(c1, c2) => agents(c1) ++ agents(c2)
    case Loop(c) => agents(c)
    case End => Set()
    case Tau => Set()
    case In(a, _, _)  => Set(a) //,b)
    case Out(a, _, _) => Set(a) //,b)

  def messages(c:Choreo): Set[Send] = c match {
    case Send(as, bs, m) =>
      for a<-as.toSet; b<-bs yield
        Send(List(a),List(b),m)
    case Seq(c1, c2) => messages(c1)++messages(c2)
    case Par(c1, c2) => messages(c1)++messages(c2)
    case Choice(c1, c2) => messages(c1)++messages(c2)
    case Loop(c2) => messages(c2)
    case End => Set()
    case Tau => Set()
    case action: Action => Set()
  }


