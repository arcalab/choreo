package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, ChorManyTausProj, PomDefProj, Projection}
import choreo.sos._
import choreo.syntax.Choreo
import choreo.syntax.Choreo.Action
import choreo.view.ViewChoreo._
import choreo.view._
import mat.frontend.Configurator
import mat.frontend.Configurator._
import mat.sos.{BranchBisim, SOS}
import mat.sos.SOS._
import mat.view._

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

  val examples: Iterable[(String,Choreo)] = Examples.examples2show

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)

  val widgets: Iterable[(Widget[Choreo],String)] = List(
    Visualize(viewPomMerm, chor2pom)
      -> "Encode Pomset",
    Visualize(viewChorMerm,id)
      -> "Sequence Diagram",
    Visualize(viewPomTxt,chor2pom)
      -> "Pomset as Text",
    Simulate(ChorBasicSOS,viewChorTxt,id)
      -> "Simulate Choreo (Basic)",
    Simulate(ChorDefSOS,viewChorTxt,id)
      -> "Simulate Choreo (Default)",
    simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id)
      -> "Simulate Network of Choreo (default)",
    simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id)
      -> "Simulate Network of Choreo (ManyTaus)",
    Simulate(PomDefSOS,viewPomMerm,chor2pom)
      -> "Simulate Pomset (default)",
    Simulate(PomKeepSOS,viewPomMerm,chor2pom)
      -> "Simulate Pomset (keeper)",
    compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom)
      -> "Choreo (def) vs Pomset (def)",
    compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
      -> "Realisability via branch-bisimulation (default proj+SOS)",
    compareTraceEq(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
      -> "Realisability via trace equivalence (default proj+SOS)",
//    Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//        yield s"- $s: "+choreo.DSL.realisable(c)).mkString("\n"))
//      -> "Default realisability of all examples",
//    Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//        yield s"- $s: "+BranchBisim.findBisim(c,chor2pom(c))(using ChorDefSOS,PomDefSOS,50).isRight).mkString("\n"))
//      -> "Choreo vs. Pomsets of all examples",
    Visualize(Text,SyntAnalysis.realisablePP)
      -> "Experiments with syntactic realisability",
//    Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
//      -> "Visualize projections of Pomsets"
    //...
  )

  val smallWidgets: Iterable[(Widget[Choreo],String)] = List(
//    compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
//      -> "Default realisability (def. projection+SOS)",
//    Visualize(Text,SyntAnalysis.realisablePP) -> "Experiments with syntactic realisability"
  )

  def simulateNet[S](sos:SOS[Action,S],
                     sview:S=>Text,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
    Simulate(Network.sos(sos),net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))


