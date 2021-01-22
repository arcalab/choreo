package choreo.choreo2.view

import choreo.choreo2.syntax.Agent
import choreo.choreo2.analysis.pomsets.Pomset
import choreo.choreo2.analysis.pomsets.Pomset._


/**
 * Created by guillecledou on 31/10/2020
 */


object DotPomsets:

  implicit object DotPomset extends Dot[Pomset]:

    private var seedId:Int = 0
    private def seed():Int = {seedId+=1;seedId-1}

    def toDot(p:Pomset): String =
      seedId = p.allEvents.max+1
      s"""
         |digraph G {
         |rankdir = "LR";
         |compound=true;
         | ${dotPomset(p)(seed())}
         |}
         |""".stripMargin

    private def dotPomset(p:Pomset)(implicit id:Int):String = 
      s"""
         |subgraph cluster_P${id} {
         | ${id} [label="",peripheries=0,height=0,width=0,style=invis];
         | ${mkRanks(p)}
         | ${p.labels.map(l=>mkLabel(l,p.labels)).mkString("\n  ")}
         | ${p.order.map(o=>mkOrder(o,p.labels)).mkString("\n  ")}
         |}
         |""".stripMargin

    private def mkRanks(p:Pomset):String = "" 
//      val eventsPerA = p.agents.map(a=> p.eventsOf(a)).map(a=>a.filter(e=>p.labels(e).simple))
//      eventsPerA.map(es=> s"""{rank=same; ${es.mkString(";")}}""" ).mkString("\n")

    private def mkLabel(l:(Event,Label),labels:Labels):String  = l._2 match {
      case LIn(b, a, m) => s"""${l._1} [label="${b.s}?${a.s}${m.pp}"]; """
      case LOut(a, b, m) => s"""${l._1} [label="${a.s}!${b.s}${m.pp}"]; """
      case Poms(ps) => 
        val pid:Set[(Int,Pomset)] = ps.map(p=> (seed(),p))
        pid.map(p=>dotPomset(p._2)(p._1)).mkString("\n") + 
        pid.map(p=>mkOrder(Order(l._1,p._1),labels)).mkString("\n")
    }

    private def mkOrder(o:Order,labels:Labels):String = (labels.get(o.left),labels.get(o.right)) match
      case (Some(ll),Some(lr)) if ll.simple && ll.simple && ll.actives.intersect(lr.actives).nonEmpty =>
        //trick to make agent block align from top to bottom  
        //s"""${o.right} -> ${o.left} [color="red" dir=back];"""
        s"""${o.left} -> ${o.right} [color="red"];"""
      case (Some(ll),Some(lr)) =>
        s"""${o.left} -> ${o.right} [color="black"];"""
      case _ => 
        s"""${o.left} -> ${o.right} [color="blue", lhead=cluster_P${o.right}];"""
