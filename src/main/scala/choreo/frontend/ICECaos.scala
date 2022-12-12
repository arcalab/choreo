package choreo.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS.*
import caos.sos.{BranchBisim, SOS}
import caos.view.*
import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.analysis.{DepGuarded, WellBranched, WellChannelled}
import choreo.api.{API, LocalAPI, Protocol, Session}
import choreo.npomsets.{Choreo2NPom, NPomDAG, NPomDefSOS, NPomset}
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.*
import choreo.realisability.CC.*
import choreo.realisability.{CC, CCPOM, ICNPOM, Merge, SyntacticFACS}
import choreo.sos.*
import choreo.sos.Network.*
import choreo.syntax.Choreo.Action
import choreo.syntax.{Agent, Choreo}
import choreo.view.*
import choreo.view.ViewChoreo.*


object ICECaos extends Configurator[(Choreo,Set[(Int,Int)])]:
  type XChoreo = (Choreo,Set[(Int,Int)])

  val name = "Branching Pomsets Encoder"
  override val languageName = "Choreography"
  /** Parser for Choreo expressions. */
  val parser: String=>XChoreo = choreo.DSL.restrParse

  val examples = List(
    "R1" -> "(a->b:yes||b->a:yes) +\n(a->b:no||b->a:no)"
      ->"R1 example from the journal paper.",
    "R2" -> "a->b:int;\n((b->a:yes + b->a:no)\n ||\n a->b:bool)"
      ->"R2 example from the journal paper.",
    "R3" -> "a->b:int; || a->b:bool\n[1->3]"
      ->"R3 example from the journal paper.",
    "R4" -> "(a->b:yes + a->b:no);\na->b:int"
      ->"R4 example from the journal paper.",
    "R4 (tree-like)" -> "(a->b:yes;a->b:int) +\n(a->b:no; a->b:int)"
      ->"R4 example from the journal paper, after moving the trailing actions inside the choice.",
    "Fig.1" -> "((c->a:r;\n (a->c:y+a->c:n) ||\n c->b:r;\n (b->c:y+b->c:n)\n) + 0)\n||\nc->a:d || c->b:d\n\n[4->13,6->13\n,10->15,12->15]",
//    "loop" -> "(a->b:x+b->a:y)*",
    "Buyer-seller (FACS)" -> ("b1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n" +
      "(b2->s:ok;b2->s:string;s->b2:date + b2->s:quit)")
      -> "Two-buyers-protocol",
    "Simple stream (FACS)" -> "(d->r:bool||k->r:bool);\nr->c:bool;\n(d->r:bool||k->r:bool);\nr->c:bool"
      -> "Simple streaming protocol",
    "BS-ill-chan" -> ("b1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n" +
      "((b2->s:ok||b2->s:string);s->b2:date + b2->s:quit)")
      -> "Ill-channeled version of the buyer-seller protocol with parallel sends",
    "SS-ill-chan" -> "((d->r:bool||k->r:bool);\n r->c:bool)\n||\n((d->r:bool||k->r:bool);\n r->c:bool)"
      -> "Ill-channeled version of the simple streaming protocol with parallel sends",
    "MC" -> "(m->w1:t;w1->m:d) ||\n(m->w2:t;w2->m:d)"
      -> "Master-Workers protocol",
    "DV" -> "((a->b:y || a->c:y) +\n (a->b:n || a->c:n))   ||\n((b->a:y || b->c:y) +\n (b->a:n || b->c:n))   ||\n((c->a:y || c->b:y) +\n (c->a:n || c->b:n))"
      -> "Distribted Voting protocol with 3 participants",
    "Race" -> "(\n (ctr->r1: start ||\n  ctr->r2: start);\n (r1->r1:run||\n  r2->r2:run); \n (r1->ctr: finish;r1->r1:rest ||\n  r2->ctr: finish;r2->r2:rest)\n)*"
      -> "Two runners in a race with a controller.",
    "Ex.1.1" -> "(a->b:x + a->c:x);\n(d->b:x + d->e:x)",
    "Ex.1.2" -> "(a->b:x + c->b:x)* ||\n(c->a:x + c->b:x)",
    "Ex.2.1 (not dep-guard)"-> "(a->b:x + a->c:x)*"->"Not dependently guarded example",
    "Ex.2.2 (dep-guard)"-> "(a->b:x + b->a:x)*"->"Dependently guarded example",
    "Fig.5" -> "a->b:x;\n(b->c:x+b->d:x);\nc->d:x",
    "Fig.6" -> "((a->b:x ;(b->a:x + b->d:x))+\n(a->c:x ;(c->a:x + c->d:x))) ; d->a:x",
    "Ex.4.1" -> "a->b:x;\n(b->a:x + b->a:y)",
    "Ex.4.2" -> "(a->b:x ; b->a:x)+\n(a->b:x ; b->a:y)",
    "Ex.4.3" -> "a->b:x + a->b:x",
    "Special pomset" -> "a->b:x  || a->b:y\n// where ab!x ≤ ab!y\n// but ab?y ≤ ab?x.\n[1->3, 4->2]",
) :::
    Examples.examples2show.map(xy => toExample(xy._1 -> xy._2.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)
  private def chor2npom(xc:XChoreo):NPomset = Choreo2NPom(xc._1) + xc._2.map(_.swap)

  import WellBranched.{show, toBool}

  override val smallWidgets = List(
    "Sequence Diagram (w/o extension)" -> view(xc => SequenceChart(xc._1), Mermaid),
    "Extension" -> view(xc => xc._2.mkString(" / "), Text),
  )
  val widgets = List(
//    "Well-Branched (FACS)" ->
//      view(c=>SyntacticFACS.wellBranched(chor2npom(c)).getOrElse("OK"), Text),
//    "Well-Channelled (FACS)" ->
//      view(c => SyntacticFACS.wellChanneled(chor2npom(c)).getOrElse("OK"), Text),
//    "Tree-like (FACS)" ->
//      view(c => SyntacticFACS.treeLike(chor2npom(c)).getOrElse("OK"), Text),
    "Global B-Pomset"
      -> view(xc => MermaidNPomset(chor2npom(xc)), Mermaid),
    "Well-formed (FACS)" ->
      view(c =>
        //val c = xc._1
        val wb = SyntacticFACS.wellBranched(chor2npom(c))
        val wc = SyntacticFACS.wellChanneled(chor2npom(c))
        val tl = SyntacticFACS.treeLike(chor2npom(c))
        val ch = SyntacticFACS.choreographic(chor2npom(c))
        if wc.isEmpty && wb.isEmpty && tl.isEmpty && ch.isEmpty then s"OK"
        else List(
          if wb.isEmpty then "well-branched" else s"NOT well-branched: ${wb.get}",
          if wc.isEmpty then "well-channeled" else s"NOT well-channeled: ${wc.get}",
          if tl.isEmpty then "tree-like" else s"NOT tree-like: ${tl.get}",
          if ch.isEmpty then "choreographic" else s"NOT choreographic: ${ch.get}"
        ).mkString("\n")
      , Text),

//    "Global B-Pomset (mermaid-txt)"
//      -> view(xc=>MermaidNPomset(chor2npom(xc)), Text),
    "Local B-Pomset"
      //      -> view( c => MermaidNPomset(chor2npom(c).projectAll), Mermaid),
      -> viewMerms((xc: XChoreo) => chor2npom(xc).projectMap.toList.map((a, b) => (a.toString, MermaidNPomset(b)))),
//    "Global LTS (from choreo)"
//      -> lts(xc=>xc._1, ChorDefSOS, _=>" ", _.toString),
    "Global LTS"
      -> lts(xc => chor2npom(xc), NPomDefSOS, _=>" ", _.toString),

//    "Local LTS (from choreo)" -> viewMerms((xc: XChoreo) =>
//      val ch = xc._1
//      for a <- Choreo.agents(ch).toList.sortWith(_.s < _.s) yield
//        a.toString -> caos.sos.SOS.toMermaid(ChorDefSOS, ChorNoTauProj.proj(ch, a), _ => " ", _.toString, 80)),
    "Local LTS" -> viewMerms((xc: XChoreo) =>
        val ch = xc._1
        for a <- Choreo.agents(ch).toList.sortWith(_.s < _.s) yield
          a.toString -> caos.sos.SOS.toMermaid(
            NPomDefSOS,
            NPomDefProj.proj(chor2npom(xc),a),
            _ => " ",
            _.toString, 80)
        ),

    "ALL: Well-Branched (FACS)" ->
      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
        name + ": " + SyntacticFACS.wellChanneled(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
        Text),
    "ALL: Well-Channelled (FACS)" ->
      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
        name + ": " + SyntacticFACS.wellChanneled(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
        Text),
    "ALL: Tree-like (FACS)" ->
      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
        name + ": " + SyntacticFACS.treeLike(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
        Text),
    "ALL: Choreographic (FACS)" ->
      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
        name + ": " + SyntacticFACS.choreographic(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
        Text),
    "ALL: Well-formed (FACS)"
      -> viewAll((cs: Seq[(String, XChoreo)]) => (for (name, c) <- cs yield
      val wb = SyntacticFACS.wellBranched(chor2npom(c))
      val wc = SyntacticFACS.wellChanneled(chor2npom(c))
      val tl = SyntacticFACS.treeLike(chor2npom(c))
      val ch = SyntacticFACS.choreographic(chor2npom(c))
      if wc.isEmpty && wb.isEmpty && tl.isEmpty && ch.isEmpty then s"$name: ok"
      else s"$name: ${
        if wb.isEmpty then "" else s"[${wb.get}] NOT "
      }well branched , ${
        if wc.isEmpty then "" else s"[${wc.get}] NOT "
      }well channeled, ${
        if tl.isEmpty then "" else s"[${tl.get}] NOT "
      }tree-like, ${
        if ch.isEmpty then "" else s"[${ch.get}] NOT "
      }choreographic."
      ).mkString("\n"),
      Text
    ),
  //    "Global pomsets" -> viewMerms(c =>
//      Choreo2NPom(c).refinements.zipWithIndex.map((p, n) => s"Pom ${n + 1}" -> MermaidNPomset(p))),
//    "Local pomsets" -> viewMerms(c =>
//      Choreo2NPom(c).refinementsProj.zipWithIndex.map((ps, n) => s"Pom ${n + 1}" -> MermaidNPomset(ps))),

    "Dependently Guarded (Choreo)"
      -> view(xc => DepGuarded(xc._1) match
          case Left(value) => s"Not dependently guarded: ${value.mkString(", ")}"
          case Right(_) => "OK"
        , Text),
    /// to drop for ICE
    "Choreo (old): Syntactic realisability"
      -> view(xc =>
            val c = xc._1
            val wb = WellBranched(c)
            val wc = WellChannelled(c)
            if wc.toBool && wb.toBool then "OK" else
              s"${if !wb.toBool then s"Not well branched:\n  - ${wb.show.drop(7)}\n" else ""}${
                  if !wc.toBool then s"Not well channeled:\n  - ${wc.show.drop(7)}\n" else ""}"
          , Text),
    "Choreo (old): ALL - Syntactic realisability"
      -> viewAll( (cs:Seq[(String,XChoreo)]) => (for (name,xc) <- cs yield
              val c = xc._1
              val wb = WellBranched(c).toBool
              val wc = WellChannelled(c).toBool
              val dg = DepGuarded(c).isRight
              if wc && wb && dg then s"$name: ok"
              else s"$name: ${if wb then "" else "NOT "}well branched, ${if wc then "" else "NOT "}well channeled, ${if dg then "" else "NOT "}dependently guarded."
            ).mkString("\n"),
            Text
          ),
//    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
//      -> compareBranchBisim(ChorDefSOS, Network.sosMS(ChorDefSOS), x => x, mkNetMS(_, ChorNoTauProj)),
//    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
//      -> compareBranchBisim(ChorDefSOS, Network.sosCS(ChorDefSOS), x => x, mkNetCS(_, ChorNoTauProj)),


//    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorNoTauProj)),
//    //    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),
//    //    "Realisability via branch-bisimulation (many-taus proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorManyTausProj)),
//    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorNoTauProj)),
//
//    "Realisability via trace equivalence (MSet) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorDefProj)),
//    "Realisability via trace equivalence (Causal) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorDefProj)),
//    //"Realisability via branch-bisimulation (NPomSOS + proj with interclosure all-in-1)"
//    //  -> compareBranchBisim(NPomDefSOS,NPomDefSOS,chor2npom,chor2npom(_).icnpom.head.getPom),
//    "Realisability via branch-bisimulation (NPomSOS + proj)"
//      -> compareBranchBisim(NPomDefSOS,Network.sosMS(NPomDefSOS),chor2npom,(c:Choreo) => mkNetMS(chor2npom(c),NPomDefProj)),
//    //"Petri Net"
//    //  -> Visualize((c:Choreo)=>View(MermaidPN(choreo.petrinet.OneSafeColouredPN.pn1)),Mermaid,id), //hardcoded pn
//    //
//
//    "CC2-NPOM NPomset Inter-Closure"
//      -> //viewMerms((c:Choreo) => for (p,id)<-chor2npom(c).icpom.zipWithIndex yield s"IC${id}" -> p),
//         VisualizeOpt(showIC, Mermaid, chor2npom(_).icnpom),
//    "CC2-NPOM-plus-plus Merge Interclosure"
//      -> view( c=> MermaidNPomset(chor2npom(c).mergeIC), Mermaid),
//        //Visualize(viewNPomMerm,Mermaid, chor2npom(_).mergeIC),
//    "CC2-NPOM NPomset (Simplified)"
//      -> view( c=> MermaidNPomset(chor2npom(c).simplifyChoices), Mermaid),
//        //Visualize(viewNPomMerm, Mermaid, chor2npom(_).simplifyChoices),
//    "CC2-NPOM NPomset Inter-Closure (Simplified)"
//      -> VisualizeOpt(showIC, Mermaid, (c:Choreo) => ICNPOM(chor2npom(c))(using true)),
//    "CC2-NPOM-plus-plus Merge Interclosure (Simplified)"
//      -> view( (c:Choreo) => MermaidNPomset(Merge.compose(ICNPOM(chor2npom(c))(using true).head)), Mermaid),
//         //Visualize(viewNPomMerm, Mermaid, (c:Choreo) => Merge.compose(ICNPOM(chor2npom(c))(using true).head)),
//    //"CC2-NPOM Summary (Simplified)"
//    //  -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2npom),
//    //"Inter-Closure (Emilio)"
//    //  -> Visualize(viewEICPomsMerm, chor2npom(_).einterclosure),
//    "CC2-POM Global Refinements"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
//    "CC2-POM Projections per Refinement"
//      -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
//    "CC2-POM Inter-Closures with Result"
//      -> VisualizeOpt(showICWithResMermaid,Mermaid,chor2npom(_).cc2),
//    "CC2-POM Summary"
//      -> view(c=> CC.ppcc2(chor2npom(c).cc2), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
//    "CC3-POM Global Prefixes"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements.map(r=>NPomDAG.prefixes(r).toList).flatten.distinct),
//    "CC3-POM Summary"
//      -> view(c=> CC.ppcc3(chor2npom(c).cc3), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),
//    //"Realisability NPomset (experiments)"
//    //  -> Visualize((b:Boolean) => Text(b.toString), chor2npom(_).realisable),
////    "Project NPomset at a"
////      -> Visualize(viewNPomMerm, chor2npom(_).project(Agent("a"))),
////    "Pomset as Text"
////      -> Visualize(viewPomTxt,chor2pom),
////    "Simulate Choreo (basic)"
////      -> Simulate(ChorBasicSOS,viewChorTxt,id),


    "Simulate Choreo (without added dependencies for b-pomsets)"
      -> steps(xc=>xc._1, ChorDefSOS, _.toString, Text),


         //Simulate(ChorDefSOS,viewChorTxt,Text,id),
//    "Simulate Network of Choreo"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,x=>x),
//    "Simulate Network of Choreo (no-taus)"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
//    "Simulate Causal Network of Choreo (no-taus)"
//      -> simulateCNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
//    "Visualize a Causal Network of Choreo (no-taus)"
//      -> view((ch:Choreo) => viewCSNetConc[Choreo](Network.mkNetCS(ChorNoTauProj.allProj(ch)), viewChorTxt).code, Text),
//         //Visualize(x=>viewCSNetConc[Choreo](x,viewChorTxt),Text, (ch:Choreo) => Network.mkNetCS(ChorNoTauProj.allProj(ch))),
////    "Simulate Network of Choreo (many-taus)"
////      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
////    "Simulate Network of Choreo (default w/o taus)"
////      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
////    "Simulate Network of Choreo (many-taus w/o taus)"
////      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),


    "Simulate B-Pomset"
      -> steps(xc=>chor2npom(xc), NPomDefSOS, MermaidNPomset.apply, Mermaid),


//    "Simulate B-Pomset (Txt)"
//      -> steps(chor2npom, NPomDefSOS, _.toString, Text),
        //Simulate(NPomDefSOS,viewNPomMerm,Mermaid,chor2npom),
//    "Simulate Pomset (keeper)"
//      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    //"Simulate NPomset Interclosure"
    // -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom(_).icnpom.head.getPom) ,
//    "Simulate B-Pomset Network"
//      -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,


    "Choreo vs B-Pomset (find bisimulation - no loops with infinte states)"
      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,xc=>xc._1,xc=>chor2npom(xc._1),200)


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
                      sview:S=>View,
                      proj:Projection[_,S],
                      enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkCausal[S]] =
    steps((c:Choreo)=> Network.mkNetCS(enc(c),proj),
      Network.sosCS(sos),
      x => ViewChoreo.viewCSNetConc(x,sview).code,
      Text)
    //Simulate(Network.sosCS(sos),net=>ViewChoreo.viewCSNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetCS(enc(c),proj))

//  def visualizeMNet[S](sview:S=>Mermaid,
//                       proj:Projection[_,S],
//                       enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
//    Visualize(net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

