package choreo.view

import caos.view.OptionView.OptMermaid
import caos.view.*
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.Order
import choreo.pomsets.Pomset
import choreo.realisability.CC.*
import choreo.realisability.{CC, Interclosure}
import choreo.sos.Network.{NetworkCausal, NetworkMS}
import choreo.syntax.Choreo
import choreo.syntax.Choreo.Action


////////////////////
// Existing views //
////////////////////
/** Views of Choreo and Pomsets as Text and Mermaid diagrams, based on [[mat.view.View]]. */
object ViewChoreo:
  def viewChorMerm(c:Choreo) = View(SequenceChart(c))//Mermaid(SequenceChart(c))
  def viewChorTxt(c:Choreo)  = View(c.toString) //Text(c.toString) //Text(SequenceChart(c))

  def viewPomTxt(p:Pomset)  = View(p.pretty) //Text(p.pretty)
  def viewPomMerm(p:Pomset) = View(MermaidPomset(p)) //Mermaid(MermaidPomset(p))
  def viewNPomMerm(p:NPomset) = View(MermaidNPomset(p)) //Mermaid(MermaidNPomset(p))
  def viewNPomsMerm(ps:Iterable[NPomset]) = View(MermaidNPomset(ps)) //Mermaid(MermaidNPomset(ps))
  def viewNPomsMermTuple(net:List[(String,NPomset)]) = OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewNPomsICMermTuple(net:List[(String,Interclosure)]) = OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewNPomsMermListTuple(net:List[(String,List[NPomset])]) =
    OptMermaid((for (n,p)<- net yield n -> MermaidNPomset(p)).toMap)
  def viewICPomsMerm(ps:(Iterable[NPomset],Order)) = View(MermaidNPomset(ps)) //Mermaid(MermaidNPomset(ps))
  //def viewEICPomsMerm(ps:(Iterable[NPomset],List[Order])) = Mermaid(MermaidNPomset.emilioIC(ps))
  //def viewCCPomRes(r:Set[CCPomLocalRes]) = Text(CCPOM.pp(r))

  def viewNetConc[S](c:NetworkMS[S],sview:S=>View): View = View(
    s"${c.proj.map(sview(_).code).mkString("  ---  ")}  ${
      if c.pending.isEmpty then "" else s"  ---  [pending:${c.pending}]"
    }")
  def viewCSNetConc[S](c:NetworkCausal[S], sview:S=>View): View = View(
    s"${c.proj.map(sview(_).code).mkString("  ---  ")}  ${
      if c.pending.isEmpty then "" else s"  ---  [pending:${
        c.pending.map(x=>s"${x._1._1}-${x._1._2}->${x._2.map(_.names).mkString(",")}").mkString("; ")}]"
    }")
  //    c.proj.map((l:S)=>sview(l)).fold(Text(""))((a,b)=>Text(a.code+" | "+b.code))
  def viewSeq[S](cs:Iterable[S],sview:S=>View) = // TEXT
    cs.map((l:S)=>sview(l)).fold(View(""))((a,b)=>View(a.code+b.code))
  // todo: fix composition of mermaids
  def viewSeqMerm[S](cs:Iterable[S],sview:S=>View) = { // MERM
    val res = cs.map((l:S)=>sview(l)).fold(View(""))((a,b)=>View(a.code+" "+b.code))
    println("--\n"+res.code)
    res
  }

  // aux
  def showRef(ref:List[NPomset]):OptMermaid =
    viewNPomsMermTuple(for (p,id)<-ref.zipWithIndex yield s"R${id}" -> p)

  def showRefProj(ref:List[List[NPomset]]):OptMermaid =
    viewNPomsMermListTuple(for (p,id)<-ref.zipWithIndex yield s"R${id}" -> p)

  def showIC(re:List[Interclosure]):OptMermaid =
    viewNPomsICMermTuple(for (p,id)<-re.zipWithIndex yield s"IC${id}" -> p)

  def showICWithResMermaid(res:CCPomInfo):OptMermaid = viewNPomsICMermTuple(
    (for (p,id)<-res.result.zipWithIndex yield
      if p._2.isDefined then s"IC${id}: OK" -> p._1
      else s"IC${id}: KO" -> p._1).toList
  )

