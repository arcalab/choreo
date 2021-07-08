package choreo.view

import choreo.syntax.Choreo
import choreo.pomsets.Pomset
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.Order
import choreo.realisability.NPomERealisability._
import choreo.sos.{Network}
import Choreo.Action
import caos.view.View
import caos.view._


////////////////////
// Existing views //
////////////////////
/** Views of Choreo and Pomsets as Text and Mermaid diagrams, based on [[mat.view.View]]. */
object ViewChoreo:
  def viewChorMerm(c:Choreo) = Mermaid(SequenceChart(c))
  def viewChorTxt(c:Choreo)  = Text(c.toString) //Text(SequenceChart(c))

  def viewPomTxt(p:Pomset)  = Text(p.pretty)
  def viewPomMerm(p:Pomset) = Mermaid(MermaidPomset(p))
  def viewNPomMerm(p:NPomset) = Mermaid(MermaidNPomset(p))
  def viewNPomsMerm(ps:Iterable[NPomset]) = Mermaid(MermaidNPomset(ps))
  def viewICPomsMerm(ps:(Iterable[NPomset],Order)) = Mermaid(MermaidNPomset(ps))
  def viewEICPomsMerm(ps:(Iterable[NPomset],List[Order])) = Mermaid(MermaidNPomset.emilioIC(ps))
  def viewECC2Pom(r:List[CC2Evidence]) = Text(r.mkString("\n"))

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

