package choreo.oldChoreo.backend

import choreo.oldChoreo.Agent
import choreo.oldChoreo.semantics.Pomset.{Event, Label, Labels, Order}
import choreo.oldChoreo.semantics.{Pomset, PomsetFamily}


/**
 * Created by guillecledou on 31/10/2020
 */


object DotPomsets:

  implicit object DotPomset extends Dot[Pomset]:
    def toDot(pf: Pomset): String =
      DotPomsetFamily.toDot(PomsetFamily(Set(pf)))

  implicit object DotPomsetFamily extends Dot[PomsetFamily]:
    private var seedId:Int = 0
    private def seed():Int = {seedId+=1;seedId-1}

    def toDot(p:PomsetFamily): String =
      seedId = 0
      s"""
         |digraph G {
         |rankdir = "LR";
         | ${p.pomsets.map(dotPomset).mkString("\n")}
         |}
         |""".stripMargin

    private def dotPomset(p:Pomset):String =
      s"""
         |subgraph cluster_P${seed()} {
         | ${p.agents.map(a=>dotAgentGraph(a,p)).mkString("\n")}
         | ${p.order.map(o=>mkOrder(o,p.labels)).mkString("\n  ")}
         |}
         |""".stripMargin

    private def dotAgentGraph(a:Agent,p:Pomset):String =
      val aLabels = p.labelsOf(a)
      s"""
         |subgraph cluster_${a.name} {
         |  style=filled;
         |  color=lightgray;
         |  label=<<b>${a.name}</b>>
         |  {rank=same; ${aLabels.keySet.mkString(";")}}
         |  ${aLabels.map(mkLabel).mkString("\n  ")}
         |}
         |""".stripMargin

    private def mkLabel(l:(Event,Label)):String  =
      s"""${l._1} [label="${Show(l._2)}"]; """

    private def mkOrder(o:Order,labels:Labels):String =
      var color = "orange"
      if labels(o.left).active == labels(o.right).active then
        // trick to make agent block align from top to bottom
        s"""${o.right} -> ${o.left} [color="red" dir=back];"""
      else
        if labels(o.right).passive.contains(labels(o.left).active) then color = "black"
        s"""${o.left} -> ${o.right} [color="${color}"];"""

