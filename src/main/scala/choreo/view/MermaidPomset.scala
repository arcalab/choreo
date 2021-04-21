package choreo.view

import cats.data.State
import choreo.pomsets._
import choreo.pomsets.Pomset._
import choreo.pomsets.Label._
import choreo.syntax.Agent
import choreo.syntax.Choreo._

/**
 * Created by guillecledou on 10/02/2021
 */

object MermaidPomset:

  //type St[A] = State[Int,A]
  private var seedId:Int = 0 // todo: change to a State modand
  private def seed():Int = {seedId+=1;seedId-1}
  
  def apply(p:Pomset):String = 
    seedId = 0
    s"""
      |flowchart TB
      | classDef lbl fill:#fff;
      | ${mkPomset(p.reduce)(using seed())}
      |""".stripMargin
  
  def mkPomset(p:Pomset)(using pid:Int):String =
    s"""
       |subgraph P$pid ${ if p.loop then "[Loop]" else if p == Pomset.identity then "[0]" else "[ ]"}
       |style P$pid fill:#fff,stroke:black
       |${p.labels.filter(l=>p.uniqueEvents.contains(l._1)).map(l=>mkLbl(l._1,l._2)).mkString("\n")}
       |${p.order.filter(o=>p.uniqueOrders.contains(o)).map(o=>mkOrder(o)).mkString("\n")}
       |end
       |""".stripMargin

  def mkOrder(o:Order):String =
    s"""${o.left} --> ${o.right}"""

  def mkLbl(e:Event,lbl:Label):String = lbl match
      case LAct(In(b,a,m)) => s"""$e(${a.s}${b.s}?${m.pp}):::lbl"""
      case LAct(Out(a,b,m)) => s"""$e(${a.s}${b.s}!${m.pp}):::lbl"""
      case LPoms(ps) => s"""$e(( ))\n""" + mkSubGraph(ps,e)
      case _ => ""

  def mkSubGraph(poms:Set[Pomset],e:Event):String =
    val pid:Set[(Int,Pomset)] = poms.map(p=> (seed(),p))
    val subPoms = pid.map(p=>mkPomset(p._2)(using p._1)).mkString("\n ")
    val e2subPoms = pid.map(p =>s"""$e -.-> P${p._1}""").mkString("\n ")
    // if only the terminal pomset, simplify graph
    if poms.size == 1 && poms.head == Pomset.identity then
      //s"""${subPoms}
      //   |${e2subPoms}
      //   |""".stripMargin
      ""
      else
        s"""
           |subgraph C$e [ Choice ]
           | style C$e fill:#ececff,stroke:#ececff
           | ${subPoms}
           |end
           | ${e2subPoms}
           |""".stripMargin
