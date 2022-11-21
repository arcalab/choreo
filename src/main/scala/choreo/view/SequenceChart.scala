package choreo.view

import choreo.sos.ChorDefSOS
import choreo.syntax.{Choreo, Msg}
import choreo.syntax.Choreo._

/**
 * Created by guillecledou on 10/02/2021
 */

object SequenceChart:

  def apply(c:Choreo):String = 
    s"""
     |sequenceDiagram
     |${seqGraph(c)}
     |""".stripMargin

  private def seqGraph(c:Choreo):String = c match
    case s@Send(snd,rcv,ms) =>
      val interactions = mkInteraction(s)
      if interactions.size>1 then
        s"""rect rgb(238, 238, 238)
           |${interactions.mkString("\n")}
           |end
           |""".stripMargin
      else interactions.mkString("\n")
    case Seq(c1, c2) =>
      s""" ${seqGraph(c1)}
         | ${seqGraph(c2)}""".stripMargin
    case Choice(c1, c2) =>
      s"""alt
         |  ${seqGraph(c1)}
         |else
         |  ${seqGraph(c2)}
         |end""".stripMargin
    case DChoice(c1, c2) => //todo: discuss how to represent this
      s"""alt 1-delayed choice
         |  ${seqGraph(c1)}
         |else
         |  ${seqGraph(c2)}
         |end""".stripMargin  
    case Par(c1, c2) =>
      s"""par
         | ${seqGraph(c1)}
         |and
         | ${seqGraph(c2)}
         |end""".stripMargin
    case Loop(c) =>
      s"""loop
         |  ${seqGraph(c)}
         |end""".stripMargin
    case In(a,b,m) =>
      mkInteraction(Send(b::Nil,a::Nil,m)).mkString("\n")
    case Out(a,b,m) =>
      mkInteraction(Send(a::Nil,b::Nil,m)).mkString("\n")
    case Internal(a,m) =>
      s"""Note over $a: ${m.l.mkString("/")}"""
    case _ => ""
  
  def mkInteraction(send:Send):List[String] =
    for s<-send.as;r<-send.bs
      yield if send.m.l.isEmpty
        then s"""${s} ->> ${r}: """
        else s"""${s} ->> ${r} :${send.m.l.mkString("/")}"""

