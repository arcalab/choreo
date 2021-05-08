package choreo.view

import choreo.syntax.Choreo
import choreo.pomsets.Pomset
import choreo.sos.{Network}
import Choreo.Action
import mat.view.View
import mat.view._

////////////////////
// Existing views //
////////////////////
/** Views of Choreo and Pomsets as Text and Mermaid diagrams, based on [[mat.view.View]]. */
object ViewChoreo:
  def viewChorMerm(c:Choreo) = Mermaid(SequenceChart(c))
  def viewChorTxt(c:Choreo)  = Text(c.toString) //Text(SequenceChart(c))

  def viewPomTxt(p:Pomset)  = Text(p.toString)
  def viewPomMerm(p:Pomset) = Mermaid(MermaidPomset(p))

  def viewNetConc[S](c:Network[S],sview:S=>Text): Text = Text(
    s"${c.proj.map(sview(_).code).mkString("  ---  ")}  ${
      if c.pending.isEmpty then "" else s"  ---  [pending:${c.pending}]"
    }")
  //    c.proj.map((l:S)=>sview(l)).fold(Text(""))((a,b)=>Text(a.code+" | "+b.code))
  def viewSeq[S](cs:Iterable[S],sview:S=>Text) =
    cs.map((l:S)=>sview(l)).fold(Text(""))((a,b)=>Text(a.code+b.code))
  // todo: fix composition of mermaids
  def viewSeqMerm[S](cs:Iterable[S],sview:S=>Mermaid) = {
    val res = cs.map((l:S)=>sview(l)).fold(Mermaid(""))((a,b)=>Mermaid(a.code+" "+b.code))
    println("--\n"+res.code)
    res
  }

