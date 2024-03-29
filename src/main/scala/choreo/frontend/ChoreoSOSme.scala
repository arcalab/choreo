package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, ChorManyTausProj, ChorNoTauProj, NPomDefProj, PomDefProj, Projection}
import choreo.sos.*
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.Action
import choreo.view.ViewChoreo.*
import choreo.view.*
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.sos.{BranchBisim, SOS}
import caos.sos.SOS.*
import Network.*
import caos.common.Example
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.view.*
import choreo.analysis.{DepGuarded, WellBranched, WellChannelled}
import choreo.api.{API, LocalAPI, Protocol, Session}
import choreo.npomsets.{Choreo2NPom, NPomDAG, NPomDefSOS, NPomset}
import choreo.realisability.{CC, CCPOM, ICNPOM, Merge}
import choreo.realisability.CC.*

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

object ChoreoSOSme extends Configurator[Choreo]:
  val name = "Choreo"
  /** Parser for Choreo expressions. */
  val parser: String=>Choreo = choreo.DSL.parse

  val examples = ("3sum" -> "(a->b:x+a->b:y+a->b:z);b->a") :: Examples.examples //examples2show.map((s,c)=>(s,c.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)

  import WellBranched.show
  import WellBranched.toBool

  val widgets = List(
//    "Encode Pomset"
//      -> Visualize(viewPomMerm, chor2pom),
    "Encode NPomset"
      -> view(c=>MermaidNPomset(chor2npom(c)), Mermaid),
//        Visualize(viewNPomMerm,Mermaid,chor2npom),
    "Sequence Diagram"
      -> view(SequenceChart.apply, Mermaid),
//      Visualize(viewChorMerm,Mermaid,id),
    "LTS Choreo (default)"
      -> lts(c=>c, ChorDefSOS, _.toString, _.toString),
    //"Scala APIs"
    //  -> VisualizeTab(
    //  (prots:Set[LocalAPI])=>prots.map(p=>View(p.toString)).toList,
    //  Text,
    //  (prots:Set[LocalAPI])=>prots.map(p=>p.set.api.name).toList,
    //  (c:Choreo)=>Protocol(c)
    //),
    ////////////////////////
    /// adapt from here! ///
    "Scala APIs"
      -> Configurator.viewTabs(
          c =>
            val s: Session = Session(chor2npom(c))
            s.modulesToCode ++ List("All"->s.toString),
          Text
         ),
//    VisualizeTab(
//      (s:Session)=>s.modulesToCode.map(m=>View(m._2)):+View(s.toString),
//      Text,
//      (s:Session)=>s.modulesToCode.map(p=>p._1):+"All",
//      (c:Choreo)=>Session(chor2npom(c))
//    ),


//    "NPomset as Text"
//      -> Visualize((p:NPomset)=>Text(p.toString),chor2npom),
//    "Simulate NPomset"
//      -> Simulate(NPomDefSOS,(p:NPomset)=>Text(p.toString),chor2npom),
    "Project NPomset"
      -> view( c => MermaidNPomset(chor2npom(c).projectAll), Mermaid),
//         Visualize(viewNPomsMerm, Mermaid, chor2npom(_).projectAll),

//    "Well-branched (choreo: default proj+SOS)"
//      -> view((c:Choreo)=>WellBranched(c).show,Text),
//    "Well-channelled (choreo: default proj+SOS)"
//      -> view((c:Choreo)=>WellChannelled(c).show,Text),
//    "Realisability (syntactically)"
//      -> viewTabs((c:Choreo) =>
//        val wb = WellBranched(c)
//        val wc = WellChannelled(c)
//        List("Summary" -> (if wc.toBool && wb.toBool then "OK" else
//          s"${if !wb.toBool then s"Not well branched: ${wb.show}\n" else ""}${
//              if !wc.toBool then s"Not well channeled: ${wc.show}\n" else ""}"
//          ),"Well-branched"->wb.show , "Well-Channelled"-> wc.show),Text),
    "Dependently Guarded"
      -> view(c => DepGuarded(c) match
          case Left(value) => s"Not dependently guarded: ${value.mkString(", ")}"
          case Right(_) => "OK"
        , Text),
    "Realisability (syntactically)"
      -> view(c =>
            val wb = WellBranched(c)
            val wc = WellChannelled(c)
            if wc.toBool && wb.toBool then "OK" else
              s"${if !wb.toBool then s"Not well branched:\n  - ${wb.show.drop(7)}\n" else ""}${
                  if !wc.toBool then s"Not well channeled:\n  - ${wc.show.drop(7)}\n" else ""}"
          , Text),
    "Realisability of all examples (syntactically)"
      -> viewAll( (cs:Seq[(String,Choreo)]) => (for (name,c) <- cs yield
            val wb = WellBranched(c).toBool
            val wc = WellChannelled(c).toBool
            val dg = DepGuarded(c).isRight
            if wc && wb && dg then s"$name: ok"
            else s"$name: ${if wb then "" else "NOT "}well branched, ${if wc then "" else "NOT "}well channeled, ${if dg then "" else "NOT "}dependently guarded."
          ).mkString("\n"),
          Text
    ),
    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
      -> compareBranchBisim(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorNoTauProj)),
    //    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),
    //    "Realisability via branch-bisimulation (many-taus proj+SOS w/o taus)"
    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorManyTausProj)),
    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
      -> compareBranchBisim(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorNoTauProj)),

    "Realisability via trace equivalence (MSet) (choreo: default proj+SOS)"
      -> compareTraceEq(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorDefProj)),
    "Realisability via trace equivalence (Causal) (choreo: default proj+SOS)"
      -> compareTraceEq(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorDefProj)),
    //"Realisability via branch-bisimulation (NPomSOS + proj with interclosure all-in-1)"
    //  -> compareBranchBisim(NPomDefSOS,NPomDefSOS,chor2npom,chor2npom(_).icnpom.head.getPom),
    "Realisability via branch-bisimulation (NPomSOS + proj)"
      -> compareBranchBisim(NPomDefSOS,Network.sosMS(NPomDefSOS),chor2npom,(c:Choreo) => mkNetMS(chor2npom(c),NPomDefProj)),
    //"Petri Net"
    //  -> Visualize((c:Choreo)=>View(MermaidPN(choreo.petrinet.OneSafeColouredPN.pn1)),Mermaid,id), //hardcoded pn
    //

    "CC2-NPOM NPomset Inter-Closure"
      -> //viewMerms((c:Choreo) => for (p,id)<-chor2npom(c).icpom.zipWithIndex yield s"IC${id}" -> p),
         VisualizeOpt(showIC, Mermaid, chor2npom(_).icnpom),
    "CC2-NPOM-plus-plus Merge Interclosure"
      -> view( c=> MermaidNPomset(chor2npom(c).mergeIC), Mermaid),
        //Visualize(viewNPomMerm,Mermaid, chor2npom(_).mergeIC),
    "CC2-NPOM NPomset (Simplified)"
      -> view( c=> MermaidNPomset(chor2npom(c).simplifyChoices), Mermaid),
        //Visualize(viewNPomMerm, Mermaid, chor2npom(_).simplifyChoices),
    "CC2-NPOM NPomset Inter-Closure (Simplified)"
      -> VisualizeOpt(showIC, Mermaid, (c:Choreo) => ICNPOM(chor2npom(c))(using true)),
    "CC2-NPOM-plus-plus Merge Interclosure (Simplified)"
      -> view( (c:Choreo) => MermaidNPomset(Merge.compose(ICNPOM(chor2npom(c))(using true).head)), Mermaid),
         //Visualize(viewNPomMerm, Mermaid, (c:Choreo) => Merge.compose(ICNPOM(chor2npom(c))(using true).head)),
    //"CC2-NPOM Summary (Simplified)"
    //  -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2npom),
    //"Inter-Closure (Emilio)"
    //  -> Visualize(viewEICPomsMerm, chor2npom(_).einterclosure),
    "CC2-POM Global Refinements"
      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
    "CC2-POM Projections per Refinement"
      -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
    "CC2-POM Inter-Closures with Result"
      -> VisualizeOpt(showICWithResMermaid,Mermaid,chor2npom(_).cc2),
    "CC2-POM Summary"
      -> view(c=> CC.ppcc2(chor2npom(c).cc2), Text),
         //Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
    "CC3-POM Global Prefixes"
      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements.map(r=>NPomDAG.prefixes(r).toList).flatten.distinct),
    "CC3-POM Summary"
      -> view(c=> CC.ppcc3(chor2npom(c).cc3), Text),
         //Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),
    //"Realisability NPomset (experiments)"
    //  -> Visualize((b:Boolean) => Text(b.toString), chor2npom(_).realisable),
//    "Project NPomset at a"
//      -> Visualize(viewNPomMerm, chor2npom(_).project(Agent("a"))),
//    "Pomset as Text"
//      -> Visualize(viewPomTxt,chor2pom),
//    "Simulate Choreo (basic)"
//      -> Simulate(ChorBasicSOS,viewChorTxt,id),
    "Simulate Choreo (default)"
      -> steps(c=>c, ChorDefSOS, _.toString, Text),
         //Simulate(ChorDefSOS,viewChorTxt,Text,id),
//    "Simulate Network of Choreo (default)"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id),
    "Simulate Network of Choreo (no-taus)"
      -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
    "Simulate Causal Network of Choreo (no-taus)"
      -> simulateCNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
    "Visualize a Causal Network of Choreo (no-taus)"
      -> view((ch:Choreo) => viewCSNetConc[Choreo](Network.mkNetCS(ChorNoTauProj.allProj(ch)), viewChorTxt).code, Text),
         //Visualize(x=>viewCSNetConc[Choreo](x,viewChorTxt),Text, (ch:Choreo) => Network.mkNetCS(ChorNoTauProj.allProj(ch))),
//    "Simulate Network of Choreo (many-taus)"
//      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
//    "Simulate Network of Choreo (default w/o taus)"
//      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
//    "Simulate Network of Choreo (many-taus w/o taus)"
//      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),
    "Simulate NPomset (default)"
      -> steps(chor2npom, NPomDefSOS, MermaidNPomset.apply, Mermaid),
        //Simulate(NPomDefSOS,viewNPomMerm,Mermaid,chor2npom),
//    "Simulate Pomset (keeper)"
//      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    //"Simulate NPomset Interclosure"
    // -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom(_).icnpom.head.getPom) ,
    "Simulate NPomset Network"
      -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,
    "Choreo (def) vs NPomset (v2)"
      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,x=>x,chor2npom)
//    "Choreo (def) vs Pomset (def)"
//      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
//    "Realisability via branch-bisimulation (default proj+SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),

//    "Experiments with syntactic realisability"
//      -> Visualize(Text,SyntAnalysis.realisablePP)
//    "Default realisability of all examples"
//      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//          yield s"- $s: "+choreo.DSL.realisable(c)).mkString("\n")),
//    "Choreo vs. Pomsets of all examples"
//      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//        yield s"- $s: "+BranchBisim.findBisim(c,chor2pom(c))(using ChorDefSOS,PomDefSOS,50).isRight).mkString("\n")),
//    "Visualize projections of Pomsets"
//      -> Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
    //...
  )

  def simulateNet[S](sos:SOS[Action,S],
                     sview:S=>View,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkMS[S]] =
    steps((c:Choreo)=> Network.mkNetMS(enc(c),proj),
          Network.sosMS[S](sos),
          x => ViewChoreo.viewNetConc(x,sview).code,
          Text)
      //Simulate(Network.sosMS(sos),net=>ViewChoreo.viewNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetMS(enc(c),proj))
//
  def simulateCNet[S](sos:SOS[Action,S],
                      sView:S=>View,
                      proj:Projection[_,S],
                      enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkCausal[S]] =
    steps((c:Choreo)=> Network.mkNetCS(enc(c),proj),
      Network.sosCS(sos),
      x => ViewChoreo.viewCSNetConc(x,sView).code,
      Text)
    //Simulate(Network.sosCS(sos),net=>ViewChoreo.viewCSNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetCS(enc(c),proj))

//  def visualizeMNet[S](sview:S=>Mermaid,
//                       proj:Projection[_,S],
//                       enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
//    Visualize(net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

