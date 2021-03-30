package choreo.choreo2.analysis.pomsets

import choreo.choreo2.syntax.Agent
import choreo.choreo2.syntax.Choreo.{Action, In, Out}
import choreo.choreo2.analysis.pomsets.Pomset
import choreo.choreo2.analysis.pomsets.Label._
import choreo.choreo2.analysis.pomsets.Pomset._

/**
 * Created by guillecledou on 19/03/2021
 */

sealed trait Label:
  def agents:Set[Agent] = this match
    //case LIn(a, b,_) => Set(a, b) // todo: check if should be only a,same below for b
    //case LOut(b, a,_) => Set(b, a)
    case LAct(In(a,b,_)) => Set(a,b)
    case LAct(Out(a,b,_)) => Set(a,b)
    case LPoms(ps) => ps.flatMap(p=>p.agents)
    case _ => Set() // tau to avoid warnings

  def actives:Set[Agent] = this match
    //case LIn(a, _, _) => Set(a)
    //case LOut(b, _, _) => Set(b)
    case LAct(In(b,_,_)) => Set(b)
    case LAct(Out(a,_,_)) => Set(a)
    case LPoms(ps) => Set()
      //val minimal = ps.flatMap(p=>GlobalPomMin.min(p).map(e=>p.labels(e)))
      //val active = minimal.flatMap(l=>l.actives).toSet 
      //if active.size == 1 
      //  then active
      //  else Set()
      // ps.flatMap(p => p.labels.values.flatMap(l => l.actives).toSet)
    case _ => Set() // tau to avoid warnings

  def matchingIO(other:Label):Boolean = (this,other) match
    //case (LOut(a, to, m1), LIn(b, from, m2)) => from == a && m1 == m2
    case (LAct(Out(a, to, m1)),LAct(In(b,from,m2))) => from == a && m1 == m2
    case _ => false

  def isFinal:Boolean = this match
    case LPoms(pomsets) => pomsets.forall(p => 
      p == identity || p.labels.values.forall(_.isFinal))
    case LAct(act) => false

  def poms():Set[Pomset] = this match
    case LPoms(ps) => ps
    case _ => Set()

  def simple:Boolean = this match
    //case LIn(_, _, _) | LOut(_, _, _) => true
    case LAct(_) => true
    case _ => false
  end simple

object Label:

  case class LPoms(pomsets: Set[Pomset]) extends Label
  
  case class LAct(act:Action) extends Label:
    
    def ->(p:Pomset):Pomset =
      val e = if p.events.nonEmpty then p.events.max+1 else 0
      Pomset(Set(e)++p.events,
        p.labels++Map(e->this),
        p.order++p.events.map(e1=>Order(e,e1)).toSet)
    end ->
    
  end LAct