package choreo.view

import cats.data.State
import choreo.npomsets.NPomset._
import choreo.npomsets._
import choreo.syntax.Agent
import choreo.syntax.Choreo._

/**
 * Created by guillecledou on 10/02/2021
 */


/** Visualisation of pomsets using Mermaid's language. */
object MermaidNPomset:

  private var seedId:Int = 0 // todo: change to a State modand
  private def nextID: Int = {seedId+=1;seedId}

  /** Generate a Mermaid diagram that represents a `NPomset` */
  def apply(p:NPomset):String =
    s"""
      |flowchart TB
      | classDef lbl fill:#fff;
      | ${mkPomset(p.simplified)}
      |""".stripMargin

  private def mkPomset(p:NPomset):String =
    val toWrap: Boolean = !isSingleton(p.events)
    val pid = nextID
    s"""
       |${if toWrap then {
            val pid = nextID
            s"""subgraph P$pid [ ]
               |style P$pid fill:#fff,stroke:black""".stripMargin}
          else ""}
       |${p.events.acts.map(mkAction(p.actions)).mkString("\n")}
       |${p.events.choices.map(c => mkChoice(p,c)).mkString("\n")}
       |${p.events.loops.map(c => mkLoop(p,c)).mkString("\n")}
       |${(for (e,es)<-p.reducedPred; e2<-es yield mkOrder(e2,e)).mkString("\n")}
       |${if toWrap then "end" else ""}
       |""".stripMargin

  private def isSingleton(events: Events) = events match
    case Nesting(as,cs,ls) => as.size+cs.size+ls.size == 1

  private def mkChoice(p: NPomset, c: NChoice[NPomset.Event]): String =
    val pid = seedId
    s"""
       |subgraph C$pid [ ]
       |style C$pid fill:#ececff,stroke:#ececff
       |${mkPomset(NPomset(c.left,p.actions,Map(),p.loop))}
       |${mkPomset(NPomset(c.right,p.actions,Map(),p.loop))}
       |end""".stripMargin

  private def mkLoop(p: NPomset, in: Events): String =
    val pid = seedId
    s"""
       |subgraph L$pid [ Loop ]
       |style L$pid fill:#fff48c,stroke:#ececff
       |${mkPomset(NPomset(in,p.actions,Map(),p.loop))}
       |end""".stripMargin

  private def mkOrder(from:Event,to:Event):String =
    s"""$from --> $to"""


  def mkAction(acts:Actions)(e:Event):String = acts(e) match
    case In(b,a,m) => s"""$e(${a.s}${b.s}?${m.pp}):::lbl"""
    case Out(a,b,m) => s"""$e(${a.s}${b.s}!${m.pp}):::lbl"""
    case Tau => s"""$e(Tau):::lbl"""

