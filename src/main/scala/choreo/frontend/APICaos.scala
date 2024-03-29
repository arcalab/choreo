package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, ChorManyTausProj, ChorNoTauProj, NPomDefProj, PomDefProj, Projection}
import choreo.sos.*
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.Action
import choreo.view.ViewChoreo
import choreo.view.ViewChoreo.*
import choreo.view.*
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.sos.{BranchBisim, SOS}
import caos.sos.SOS.*
import caos.frontend.widgets.WidgetInfo
import Network.*
import caos.view.*
import choreo.analysis.{DepGuarded, WellBranched, WellChannelled}
import choreo.api.{API, LocalAPI, Protocol, Session}
import choreo.npomsets.{Choreo2NPom, NPomDAG, NPomDefSOS, NPomset}
import choreo.realisability.{CC, CCPOM, ICNPOM, Merge}
import choreo.realisability.CC.*
import WellBranched.toBool

//object ChoreoSOSme extends Configurator[Choreo]:
//  val name     = "Choreo"
//  val parser   = choreoParser
//  val examples = Examples.examples2show
//  val widgets  = List(
//    Visualize(viewPomMerm,Choreo2Pom(_))
//      -> "Pomset Encoding",
//    Visualize(viewChorMerm,id)
//      -> "Sequence Diagram",
//    Simulate(PomDefSOS,viewPomMerm,chor2pom)
//      -> "Pomset Simulation"
//    //...
//  )

object APICaos extends Configurator[Choreo]:
  val name = "Scala API generation of MPST via Pomsets"
  override val languageName: String = "Global Protocol"
  /** Parser for Choreo expressions. */
  val parser: String=>Choreo = choreo.DSL.parse

  val examples = choreo.api.Examples.examples //examples2show.map((s,c)=>(s,c.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)

  import WellBranched.show

  val widgets = List(
    //"Encode NPomset"
    //  -> Visualize(viewNPomMerm,Mermaid,chor2npom),
    //"Project NPomset"
    //  -> Visualize(viewNPomsMerm, Mermaid, chor2npom(_).projectAll),
    "Sequence Diagram"
      -> view(SequenceChart.apply,Mermaid), //Visualize(viewChorMerm,Mermaid,id),
//    "Global Set of Pomsets"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
//    "Projected Set of Pomsets"
//      -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
    "Global Set of Pomsets"
      -> viewMerms( c =>
          chor2npom(c).refinements.zipWithIndex.map((p,n) => s"Pom ${n+1}"->MermaidNPomset(p))),
    "Projected Set of Pomsets"
      -> viewMerms( c =>
          chor2npom(c).refinementsProj.zipWithIndex.map((ps,n) => s"Pom ${n+1}"->MermaidNPomset(ps))),
//    "Weak Realisability - CC2-POM "
//      -> view(c => CC.ppcc2(chor2npom(c).cc2), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
//    "Safe Realisability - CC3-POM "
//      -> view(c => CC.ppcc3(Choreo2NPom(c).cc3), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),
    "Scala APIs"
      -> viewTabs((c:Choreo) =>
      val session = Session(chor2npom(c));
      session.modulesToCode:::List("All"->session.toString),
      Text),
     "Realisability via CC2 & CC3"
      -> viewTabs( (c:Choreo) => List(
          "Summary" -> {
              val (cc2,cc3) = (CC.iscc2(chor2npom(c).cc2),CC.iscc3(chor2npom(c).cc3))
              s"The global choreography${if cc2 && cc3 then " IS" else " is NOT"} fully realisable."+
                (if !cc2 then s"\n  - it is NOT weakly realisable" else "")+
                (if !cc3 then s"\n  - it is NOT safely realisable" else "")},
          "Weak Realisability (CC2-Pom)" -> CC.ppcc2(chor2npom(c).cc2),
          "Safe Realisability (CC2-Pom)" -> CC.ppcc3(chor2npom(c).cc3)
          ), Text),
    "Realisability via syntactic checks (sound but not complete)"
      -> view( (c:Choreo) =>
      val wb = WellBranched(c)
      val wc = WellChannelled(c)
      val dg = DepGuarded(c)
      if wc.toBool && wb.toBool && dg.isRight then "OK" else
        s"${if !dg.isRight then s"Not dependently guarded:\n  - ${dg.fold(_.mkString(","),_=>"")}\n" else ""}${
          if !wb.toBool then s"Not well branched:\n  - ${wb.show.drop(7)}\n" else ""}${
          if !wc.toBool then s"Not well channeled:\n  - ${wc.show.drop(7)}\n" else ""}"
      , Text),
    "Realisability of ALL examples via syntactic checks"
      -> viewAll( (cs:Seq[(String,Choreo)]) => (for (name,c) <- cs yield
      val wb = WellBranched(c).toBool
      val wc = WellChannelled(c).toBool
      val dg = DepGuarded(c).isRight
      if wc && wb && dg then s"$name: ok"
      else s"$name: ${if wb then "" else "NOT "}well branched, ${if wc then "" else "NOT "}well channeled, ${if dg then "" else "NOT "}dependently guarded."
      ).mkString("\n"),
      Text
    )
//  "Scala APIs"
//     ->  VisualizeTab(
//      (s:Session)=> s.modulesToCode.map(m=>View(m._2)):+View(s.toString),
//      //(s:Session)=> s.modulesToCode.map(m=>View(choreo.api.Examples.dummyCode)):+View(s.toString),
//      Text,
//      (s:Session)=>s.modulesToCode.map(p=>p._1):+"All",
//      (c:Choreo)=>Session(chor2npom(c))
//      )

    //"Simulate NPomset Network"
    //  -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,


    //    "NPomset as Text"
    //      -> Visualize((p:NPomset)=>Text(p.toString),chor2npom),
    //    "Simulate NPomset"
    //      -> Simulate(NPomDefSOS,(p:NPomset)=>Text(p.toString),chor2npom),


    //"Well-branched (choreo: default proj+SOS)"
    //  -> Visualize((c:Choreo)=>View(WellBranched(c).show),Text,id),
    //"Well-channelled (choreo: default proj+SOS)"
    //  -> Visualize((c:Choreo)=>View(WellChannelled(c).show),Text,id),
    //"Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
    //  -> compareBranchBisim(ChorDefSOS,Network.sosMS(ChorDefSOS),id,mkNetMS(_,ChorNoTauProj)),
    ////    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
    ////      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),
    ////    "Realisability via branch-bisimulation (many-taus proj+SOS w/o taus)"
    ////      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorManyTausProj)),
    //"Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
    //  -> compareBranchBisim(ChorDefSOS,Network.sosCS(ChorDefSOS),id,mkNetCS(_,ChorNoTauProj)),
    //
    //"Realisability via trace equivalence (MSet) (choreo: default proj+SOS)"
    //  -> compareTraceEq(ChorDefSOS,Network.sosMS(ChorDefSOS),id,mkNetMS(_,ChorDefProj)),
    //"Realisability via trace equivalence (Causal) (choreo: default proj+SOS)"
    //  -> compareTraceEq(ChorDefSOS,Network.sosCS(ChorDefSOS),id,mkNetCS(_,ChorDefProj)),
    ////"Realisability via branch-bisimulation (NPomSOS + proj with interclosure all-in-1)"
    ////  -> compareBranchBisim(NPomDefSOS,NPomDefSOS,chor2npom,chor2npom(_).icnpom.head.getPom),
    //"Realisability via branch-bisimulation (NPomSOS + proj)"
    //  -> compareBranchBisim(NPomDefSOS,Network.sosMS(NPomDefSOS),chor2npom,(c:Choreo) => mkNetMS(chor2npom(c),NPomDefProj)),
    //
    ////"Petri Net"
    ////  -> Visualize((c:Choreo)=>View(MermaidPN(choreo.petrinet.OneSafeColouredPN.pn1)),Mermaid,id), //hardcoded pn
    ////
    //
    ////"CC2-NPOM NPomset Inter-Closure"
    ////  -> VisualizeOpt(showIC, Mermaid, chor2npom(_).icnpom),
    ////"CC2-NPOM-plus-plus Merge Interclosure"
    ////  -> Visualize(viewNPomMerm,Mermaid, chor2npom(_).mergeIC),
    ////"CC2-NPOM NPomset (Simplified)"
    ////  -> Visualize(viewNPomMerm, Mermaid, chor2npom(_).simplifyChoices),
    ////"CC2-NPOM NPomset Inter-Closure (Simplified)"
    ////  -> VisualizeOpt(showIC, Mermaid, (c:Choreo) => ICNPOM(chor2npom(c))(using true)),
    ////"CC2-NPOM-plus-plus Merge Interclosure (Simplified)"
    ////  -> Visualize(viewNPomMerm, Mermaid, (c:Choreo) => Merge.compose(ICNPOM(chor2npom(c))(using true).head)),
    ////
    ////"CC2-POM Global Refinements"
    ////  -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
    ////"CC2-POM Projections per Refinement"
    ////  -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
    ////"CC2-POM Inter-Closures with Result"
    ////  -> VisualizeOpt(showICWithResMermaid,Mermaid,chor2npom(_).cc2),
    ////"CC2-POM Summary"
    ////  -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
    ////"CC3-POM Global Prefixes"
    ////  -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements.map(r=>NPomDAG.prefixes(r).toList).flatten.distinct),
    ////"CC3-POM Summary"
    ////  -> Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),

    //"Simulate Choreo (default)"
    //  -> Simulate(ChorDefSOS,viewChorTxt,Text,id),
    ////    "Simulate Network of Choreo (default)"
    ////      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id),
    //"Simulate Network of Choreo (no-taus)"
    //  -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,id),
    //"Simulate Causal Network of Choreo (no-taus)"
    //  -> simulateCNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,id),
    //"Visualize a Causal Network of Choreo (no-taus)"
    //  -> Visualize(x=>viewCSNetConc[Choreo](x,viewChorTxt),Text, (ch:Choreo) => Network.mkNetCS(ChorNoTauProj.allProj(ch))),
    ////    "Simulate Network of Choreo (many-taus)"
    ////      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
    ////    "Simulate Network of Choreo (default w/o taus)"
    ////      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
    ////    "Simulate Network of Choreo (many-taus w/o taus)"
    ////      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),
    //"Simulate NPomset (default)"
    //  -> Simulate(NPomDefSOS,viewNPomMerm,Mermaid,chor2npom),
    ////    "Simulate Pomset (keeper)"
    ////      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    ////"Simulate NPomset Interclosure"
    //// -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom(_).icnpom.head.getPom) ,
    //"Simulate NPomset Network"
    //  -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,
    //"Choreo (def) vs NPomset (v2)"
    //  -> compareBranchBisim(ChorDefSOS,NPomDefSOS,id,chor2npom)
    ////    "Choreo (def) vs Pomset (def)"
    ////      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
    ////    "Realisability via branch-bisimulation (default proj+SOS)"
    ////      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),
    //
    ////    "Experiments with syntactic realisability"
    ////      -> Visualize(Text,SyntAnalysis.realisablePP)
    ////    "Default realisability of all examples"
    ////      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
    ////          yield s"- $s: "+choreo.DSL.realisable(c)).mkString("\n")),
    ////    "Choreo vs. Pomsets of all examples"
    ////      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
    ////        yield s"- $s: "+BranchBisim.findBisim(c,chor2pom(c))(using ChorDefSOS,PomDefSOS,50).isRight).mkString("\n")),
    ////    "Visualize projections of Pomsets"
    ////      -> Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
    ////...
  )

  def simulateNet[S](sos:SOS[Action,S],
    sview:S=>View,
    proj:Projection[_,S],
    enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkMS[S]] =
//    Simulate(Network.sosMS(sos),net=>ViewChoreo.viewNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetMS(enc(c),proj))
    Configurator.steps(
      initialSt = (c:Choreo)=>Network.mkNetMS(enc(c),proj),
      Network.sosMS(sos),
      net=>ViewChoreo.viewNetConc(net,sview).code,
      Text
    )

  def simulateCNet[S](sos:SOS[Action,S],
    sview:S=>View,
    proj:Projection[_,S],
    enc:(Choreo=>S)): WidgetInfo[Choreo] =  //Simulate[Choreo,Action,NetworkCausal[S]] =
//    Simulate(Network.sosCS(sos),net=>ViewChoreo.viewCSNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetCS(enc(c),proj))
    Configurator.steps(
      initialSt = (c:Choreo)=>Network.mkNetCS(enc(c),proj),
      Network.sosCS(sos),
      net=>ViewChoreo.viewCSNetConc(net,sview).code,
      Text
    )

//  def visualizeMNet[S](sview:S=>Mermaid,
//                       proj:Projection[_,S],
//                       enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
//    Visualize(net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

