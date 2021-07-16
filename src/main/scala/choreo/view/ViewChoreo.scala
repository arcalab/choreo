package choreo.view

import choreo.syntax.Choreo
import choreo.pomsets.Pomset
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.Order
import choreo.sos.Network
import Choreo.Action
import caos.view.OptionView.OptMermaid
import caos.view.View
import caos.view._
import choreo.realisability.{CCPOM, Interclosure}
import choreo.realisability.CCPOM.{CCPomLocalRes,CCPomInfo}


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
  def viewNPomsMermTuple(net:List[(String,NPomset)]) = OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewNPomsICMermTuple(net:List[(String,Interclosure)]) = OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewNPomsMermListTuple(net:List[(String,List[NPomset])]) =
    OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewICPomsMerm(ps:(Iterable[NPomset],Order)) = Mermaid(MermaidNPomset(ps))
  //def viewEICPomsMerm(ps:(Iterable[NPomset],List[Order])) = Mermaid(MermaidNPomset.emilioIC(ps))
  //def viewCCPomRes(r:Set[CCPomLocalRes]) = Text(CCPOM.pp(r))

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

  // aux
  def showRef(ref:List[NPomset]):OptMermaid =
    viewNPomsMermTuple(for (p,id)<-ref.zipWithIndex yield s"R${id}" -> p)

  def showRefProj(ref:List[List[NPomset]]):OptMermaid =
    viewNPomsMermListTuple(for (p,id)<-ref.zipWithIndex yield s"R${id}" -> p)

  def showIC(re:List[Interclosure]):OptMermaid =
    viewNPomsMermTuple(for (p,id)<-re.zipWithIndex yield s"IC${id}" -> p.getPom)

  def showICWithResMermaid(res:CCPomInfo):OptMermaid = viewNPomsICMermTuple(
    (for (p,id)<-res.result.zipWithIndex yield
      if p._2.isDefined then s"IC${id}: OK" -> p._1
      else s"IC${id}: KO" -> p._1).toList
  )

