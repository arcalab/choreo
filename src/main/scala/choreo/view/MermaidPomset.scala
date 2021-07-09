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


/** Visualisation of pomsets using Mermaid's language. */
object MermaidPomset:

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
       |${p.labels.filter(l => p.uniqueEvents.contains(l._1)).map(l => mkLbl(l._1, l._2)).mkString("\n")}
       |${p.order.filter(o=> // hide arrows from and to labels with NON-EMPTY pomsets
            (!p.labels.exists(toHide(o.left))) && // hide depdendency from choice-event
            (!p.labels.exists(toHide(o.right))) && // hide depdendency from choice-event
//          !p.labels.exists(el=>el._1==o.left  && !el._2.simple) && // hide depdendency from choice-event
//          !p.labels.exists(el=>el._1==o.right && !el._2.simple) && // hide depdendency from choice-event
            p.uniqueOrders.contains(o)).map(o=>mkOrder(o)).mkString("\n")}
       |end
       |""".stripMargin

  private def toHide(ev: Event)(el: (Event, Label)) =
    el._1==ev  && !el._2.simple && el._2.poms()!=Set(Pomset.identity)

  def mkOrder(o:Order):String =
    s"""${o.left} --> ${o.right}"""

  def mkLbl(e:Event,lbl:Label):String = lbl match
      case LAct(In(b,a,m)) => s"""$e($e:${a.s}${b.s}?${m.pp}):::lbl"""
      case LAct(Out(a,b,m)) => s"""$e($e:${a.s}${b.s}!${m.pp}):::lbl"""
      case LPoms(ps) => (if ps==Set(Pomset.identity) then s"$e(( ))\n" else "") +  // dropping choice-event
                        mkSubGraph(ps,e)
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
           |end""".stripMargin
//           | ${e2subPoms}
//           |""".stripMargin
