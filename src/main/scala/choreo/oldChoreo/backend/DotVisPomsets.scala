package choreo.oldChoreo.backend

import choreo.oldChoreo.Agent._
import choreo.oldChoreo.Agent
import choreo.oldChoreo.semantics.Pomset.{Event, Label, Labels, Order}
import choreo.oldChoreo.semantics.{Pomset, PomsetFamily}

/**
  * Created by guillecledou on 03/11/2020
  */


object DotVisPomsets:

  implicit object DotVisPomset extends Dot[Pomset]:
    def toDot(pf: Pomset): String =
      DotVisPomsetFamily.toDot(PomsetFamily(Set(pf)))

  implicit object DotVisPomsetFamily extends Dot[PomsetFamily]:
    private var seedId:Int = 0
    private def seed():Int = {seedId+=1;seedId-1}

    def toDot(p:PomsetFamily): String =
      seedId = 0
      s"""
         |digraph G {
         |rankdir = "LR";
         |node [style = "rounded" shape ="box"]
         | ${p.pomsets.map(dotPomset).mkString("\n")}
         |}
         |""".stripMargin

    private def dotPomset(p:Pomset):String =
      s""" ${p.agents.map(a=>dotAgentGraph(a,p)).mkString("\n")}
         | ${p.order.map(o=>mkOrder(o,p.labels)).mkString("\n  ")}""".stripMargin

    private def dotAgentGraph(a:Agent,p:Pomset):String =
      s"""${p.labelsOf(a).map(mkLabel).mkString("\n  ")}""".stripMargin

    private def mkLabel(l:(Event,Label)):String  =
      s"""${l._1} [label="${Show(l._2)}" color="#a7a7e6" style="filled" fillcolor="#ECECFF"]; """

    private def mkOrder(o:Order,labels:Labels):String =
      var color = "orange"
      if labels(o.left).active == labels(o.right).active then
        color = "#c2a566"
      if labels(o.right).passive.contains(labels(o.left).active) then
        color = "black"
      s"""s{o.left} -> ${o.right} [color="${color}" arrowhead=normal];"""

