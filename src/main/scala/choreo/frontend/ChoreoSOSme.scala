package choreo.frontend

import choreo.Examples
import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.{ChorDefProj, PomDefProj, Projection}
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

  private def id(c:Choreo):Choreo = c
  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2net[A](p:Projection[A,Choreo])(c:Choreo):Network[Choreo] =
    Network(c,p)

  val widgets: Iterable[(Widget[Choreo],String)] = List(
    Visualize(viewPomMerm, chor2pom)
      -> "Pomset Encoding",
    Visualize(viewChorMerm,id)
      -> "Sequence Diagram",
    Visualize(viewPomTxt,chor2pom)
      -> "Pomset Text",
    Simulate(ChorBasicSOS,viewChorTxt,id)
      -> "Simulate Choreo (Basic)",
    Simulate(ChorManyTausSOS,viewChorTxt,id)
      -> "Simulate Choreo (ManyTaus)",
    simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id)
      -> "Simulate Choreo Network (default)",
    Simulate(PomDefSOS,viewPomMerm,chor2pom)
      -> "Simulate Pomset",
    compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
      -> "Default realisability (def. projection+SOS)",
    Visualize(Text,SyntAnalysis.realisablePP)
      -> "Experiments with syntactic realisability",
    Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
      -> "Visualize projections of Pomsets"
    //...
  )

  val smallWidgets: Iterable[(Widget[Choreo],String)] = List(
    compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj))
      -> "Default realisability (def. projection+SOS)",
    Visualize(Text,SyntAnalysis.realisablePP) -> "Experiments with syntactic realisability"
  )

  def simulateNet[S](sos:SOS[Action,S],
                     sview:S=>Text,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
    Simulate(Network.sos(sos),net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))


