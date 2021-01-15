package choreo.oldChoreo.backend

import choreo.oldChoreo.Choreography.Interaction
import choreo.oldChoreo.Choreography

/**
 * Created by guillecledou on 01/11/2020
 */


object MermaidChoreography:

  implicit object MermaidChoreo extends Mermaid[Choreography]:

    def toMermaid(c:Choreography):String =
      s"""
         |sequenceDiagram
         |${seqGraph(c)}
         |""".stripMargin
  
    private def seqGraph(c:Choreography):String = c match
      case i@Choreography.Interaction(senders, receivers, memories, name) =>
        val interactions = mkInteraction(i)
        if interactions.size>1 then
          s"""rect rgb(238, 238, 238)
             |${interactions.mkString("\n")}
             |end
             |""".stripMargin
        else interactions.mkString("\n")
      case Choreography.Seq(c1, c2) =>
        s""" ${seqGraph(c1)}
           | ${seqGraph(c2)}""".stripMargin
      case Choreography.Choice(c1, c2) =>
        s"""alt
           |  ${seqGraph(c1)}
           |else
           |  ${seqGraph(c2)}
           |end""".stripMargin
      case Choreography.Par(c1, c2) =>
        s"""par
           | ${seqGraph(c1)}
           |and
           | ${seqGraph(c2)}
           |end""".stripMargin
      case Choreography.Loop(c) =>
        s"""loop
           |  ${seqGraph(c)}
           |end""".stripMargin
  
    def mkInteraction(i:Interaction):List[String] =
      for s<-i.senders;r<-i.receivers
        yield s"""${s.name} ->> ${r.name} :${i.channelName}(${i.memories.map(_.name).mkString(",")})"""
