package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2NPom, Choreo2Pom, NPomDefSOS, NPomset, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, ChorManyTausProj, PomDefProj, Projection}
import choreo.sos._
import choreo.syntax.Choreo
import choreo.syntax.Choreo.Action
import choreo.view.ViewChoreo._
import choreo.view._
import caos.frontend.Configurator
import caos.frontend.Configurator._
import caos.sos.{BranchBisim, SOS}
import caos.sos.SOS._
import caos.view._

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

  val examples = Examples.examples2show.map((s,c)=>(s,c.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)

  val widgets: Iterable[(String,Widget[Choreo])] = List(
    "Encode Pomset"
      -> Visualize(viewPomMerm, chor2pom),
    "Sequence Diagram"
      -> Visualize(viewChorMerm,id),
    "NPomset as Text (v2)"
      -> Visualize((p:NPomset)=>Text(p.toString),chor2npom),
    "Simulate NPomset (v2)"
      -> Simulate(NPomDefSOS,(p:NPomset)=>Text(p.toString),chor2npom),

    "Pomset as Text"
      -> Visualize(viewPomTxt,chor2pom),
    "Simulate Choreo (basic)"
      -> Simulate(ChorBasicSOS,viewChorTxt,id),
    "Simulate Choreo (default)"
      -> Simulate(ChorDefSOS,viewChorTxt,id),
    "Simulate Network of Choreo (default)"
      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id),
    "Simulate Network of Choreo (many-taus)"
      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
    "Simulate Pomset (default)"
      -> Simulate(PomDefSOS,viewPomMerm,chor2pom),
    "Simulate Pomset (keeper)"
      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    "Choreo (def) vs NPomset (v2)"
      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,id,chor2npom),
    "Choreo (def) vs Pomset (def)"
      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
    "Realisability via branch-bisimulation (default proj+SOS)"
      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),
    "Realisability via trace equivalence (default proj+SOS)"
      -> compareTraceEq(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
//    "Experiments with syntactic realisability"
//      -> Visualize(Text,SyntAnalysis.realisablePP),
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


