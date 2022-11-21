package choreo.view

import cats.data.State
import choreo.npomsets.NPomset._
import choreo.common.MRel._
import choreo.npomsets._
import choreo.realisability.Interclosure
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
  //    val res =
    s"""
       |flowchart TB
       | classDef lbl fill:#fff;
       | ${mkPomset(p.simplified)}
       |""".stripMargin
  //    println(res)
  //    res

  /** Generate a Mermaid diagram that represents a `NPomset` */
  def apply(ps:Iterable[NPomset]):String =
    s"""
       |flowchart TB
       | classDef lbl fill:#fff;
       | ${ps.map(p=>mkPomset(p.simplified)).mkString("\n")}
       |""".stripMargin

  /** Generate a Mermaid diagram that represents a network of `NPomset`
   * with interclosure relation  */
  def apply(poms:(Iterable[NPomset],Order)):String =
    val (ps,ic) = poms
    val icOrder = for (e,es)<-ic; e2<-es yield mkOrder(e2,e)
    s"""
       |flowchart TB
       | classDef lbl fill:#fff;
       |
       | ${icOrder.mkString("\n")}
       | ${LazyList.range(0, icOrder.size)
          .map(i => s"linkStyle $i stroke-width:2px,fill:none,stroke:orange;")
          .mkString("\n")
        }
       | ${ps.map(p => mkPomset(p.simplified)).mkString("\n")}
       |""".stripMargin

  private def mkPomset(p:NPomset):String =
    val toWrap: Boolean = !isSingleton(p.events)//true
//      p.events.acts.nonEmpty || (p.events.loops.nonEmpty && p.events.choices.nonEmpty)
    val pid = nextID
    s"""
       |${if toWrap then {
      val pid = nextID
      s"""subgraph P$pid [ ]
         |direction TB
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
       |direction TB
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

  private def mkOrder(from:Event,to:Event,text:Option[String]=None):String = text match
    case Some(t) => s"""$from -- "$t" --> $to"""
    case None => s"""$from --> $to"""

  def mkAction(acts:Actions)(e:Event):String = acts(e) match
    case In(b,a,m) => s"""$e($e:${a.s}${b.s}?${m.pp}):::lbl"""
    case Out(a,b,m) => s"""$e($e:${a.s}${b.s}!${m.pp}):::lbl"""
    case Internal(a,m) => s"""$e($e:${a.s}:${m.pp}):::lbl"""
    case Tau => s"""$e(Tau):::lbl"""
    case a => s"""$e($e:$a):::lbl"""

  /////////////////////////////////
  // Interclosure
  /////////////////////////////////

  /** Generate a mermaid diagram for an interclosure */
  def apply(ic:Interclosure):String =
    val icOrder = for (e,es)<-ic.ic; e2<-es yield mkOrder(e2,e)
    s"""
       |flowchart TB
       | classDef lbl fill:#fff;
       | ${icOrder.mkString("\n")}
       | ${LazyList.range(0, icOrder.size)
        .map(i => s"linkStyle $i stroke-width:2px,fill:none,stroke:orange;")
        .mkString("\n")}
       | ${mkPomset(ic.getNetNPom.simplifiedFull)}
       |""".stripMargin

  //////////////////////////////////////////////////////////////////
  // Show Roberto Guanciale, Emilio Tuosto Interclosure
  //////////////////////////////////////////////////////////////////

  /** Generate a Mermaid diagram that represents a network of `NPomset`
   * with interclosure relation  */
  def mermaidIC(poms:(Iterable[NPomset],List[Order])):String =
    val (ps,ics) = poms
    val (simpleICs,default) = separateIC(ics)
    var seed = 0
    var icOrders:List[String] = List()
    for (ic,i)<-simpleICs.zipWithIndex do
      val (text,size) = mkColorIC(ic,Some(i.toString),randColor())(using seed)
      icOrders = icOrders.appended(text)
      seed += size
    s"""
       |flowchart TB
       | classDef lbl fill:#fff;
       |
       | ${icOrders.mkString("\n")}
       | ${mkColorIC(default,if simpleICs.isEmpty then Some("0") else None,"orange")(using seed)._1}
       |
       | ${ps.map(p => mkPomset(p.simplified)).mkString("\n")}
       |""".stripMargin


  def separateIC(ics:List[Order]):(List[Set[(Event,Event)]],Set[(Event,Event)]) =
    val pairs = ics.map(ic=>asPairs(using ic))
    val default = pairs.toSet.foldRight(pairs.flatten.toSet)({case (s,ac) => s.intersect(ac)})
    (pairs.map(ic=> ic--default),default.toSet)

  def mkColorIC(ic:Set[(Event,Event)],icText:Option[String],color:String)(using seed:Int):(String,Int) =
    val icOrder = for (e1,e2)<-ic yield mkOrder(e2,e1,icText)
    val text = icOrder.mkString("\n") ++ "\n" ++
      LazyList.range(seed, seed+icOrder.size)
        .map(i => s"linkStyle $i stroke-width:2px,fill:none,stroke:${color} ;")
        .mkString("\n")
    (text,icOrder.size)

  def randColor():String =
    val letters:String = "0123456789ABCDEF"
    var color = "#"
    for i <- Range(0,6) do
      color += letters(Math.floor(Math.random() * 16).toInt)
    color

