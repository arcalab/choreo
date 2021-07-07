package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, ChorManyTausProj, ChorNoTauProj, NPomDefProj, PomDefProj, Projection}
import choreo.sos._
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.Action
import choreo.view.ViewChoreo._
import choreo.view._
import caos.frontend.Configurator
import caos.frontend.Configurator._
import caos.sos.{BranchBisim, SOS}
import caos.sos.SOS._
import caos.view._
import choreo.npomsets.{Choreo2NPom, NPomDefSOS, NPomset}
import choreo.realisability.NPomRealisability

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

  val examples = Examples.examples //examples2show.map((s,c)=>(s,c.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)

  val widgets: Iterable[(String,Widget[Choreo])] = List(
//    "Encode Pomset"
//      -> Visualize(viewPomMerm, chor2pom),
    "Encode NPomset"
      -> Visualize(viewNPomMerm, chor2npom),
    "Sequence Diagram"
      -> Visualize(viewChorMerm,id),
    "NPomset as Text"
      -> Visualize((p:NPomset)=>Text(p.toString),chor2npom),
    "Simulate NPomset"
      -> Simulate(NPomDefSOS,(p:NPomset)=>Text(p.toString),chor2npom),
    "Project NPomset"
      -> Visualize(viewNPomsMerm, chor2npom(_).projectAll),
    "Inter-Closure"
      -> Visualize(viewICPomsMerm, chor2npom(_).interclosure),
    "Inter-Closure (Emilio)"
      -> Visualize(viewEICPomsMerm, chor2npom(_).einterclosure),
    "CC2-POM (Emilio)"
      -> Visualize(viewECC2Pom,chor2npom(_).cc2),
    "Realisability NPomset (experiments)"
      -> Visualize((b:Boolean) => Text(b.toString), chor2npom(_).realisable),
//    "Project NPomset at a"
//      -> Visualize(viewNPomMerm, chor2npom(_).project(Agent("a"))),
//    "Pomset as Text"
//      -> Visualize(viewPomTxt,chor2pom),
    "Simulate Choreo (basic)"
      -> Simulate(ChorBasicSOS,viewChorTxt,id),
    "Simulate Choreo (default)"
      -> Simulate(ChorDefSOS,viewChorTxt,id),
    "Simulate Network of Choreo (default)"
      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id),
    "Simulate Network of Choreo (no-taus)"
      -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,id),
    "Simulate Network of Choreo (many-taus)"
      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
    "Simulate Network of Choreo (default w/o taus)"
      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
    "Simulate Network of Choreo (many-taus w/o taus)"
      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),
    "Simulate NPomset (default)"
      -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom),
    "Simulate Pomset (keeper)"
      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    "Choreo (def) vs NPomset (v2)"
      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,id,chor2npom),
    "Choreo (def) vs Pomset (def)"
      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
    "Realisability via branch-bisimulation (default proj+SOS)"
      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),

    "Realisability via branch-bisimulation (no-tau-proj + default SOS)"
      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorNoTauProj)),
    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),

    "Realisability via trace equivalence (default proj+SOS)"
      -> compareTraceEq(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
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
                     sview:S=>Text,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
    Simulate(Network.sos(sos),net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

//  def visualizeMNet[S](sview:S=>Mermaid,
//                       proj:Projection[_,S],
//                       enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
//    Visualize(net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

