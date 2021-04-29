package choreo.frontend

import choreo.analysis.Bisimulation
import choreo.analysis.other.SyntAnalysis
import choreo.frontend.Arcatools.{Compare, Simulate, Visualize, Widget}
import choreo.pomsets.{Choreo2Pom, PomDefSOS, Pomset}
import choreo.projection.{ChorDefProj, Projection}
import choreo.sos._
import choreo.syntax.Choreo
import choreo.syntax.Choreo.Action
import choreo.view._
import choreo.view.View._
import choreo.{sos, projection => proj}

object ChoreoAC extends Arcatools[Choreo]:
  val name = "Choreo"
  /** Parser for Choreo expressions. */
  val parser: String=>Choreo = choreo.DSL.parse

  private def id(c:Choreo):Choreo = c
  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2net[A](p:Projection[A,Choreo])(c:Choreo):Network[Choreo] =
    Network(c,p)

  val widgets: Iterable[(Widget[Choreo],String)] = List(
    Visualize(viewChorMerm,id) -> "Sequence Diagram",
    Simulate(ChorDefSOS, viewChorTxt,id) -> "Simulate Choreo (Default)",
    Simulate(ChorBasicSOS,viewChorTxt,id) -> "Simulate Choreo (Basic)",
    Simulate(ChorManyTausSOS,viewChorTxt,id) -> "Simulate Choreo (ManyTaus)",
    simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,id) -> "Simulate Choreo Network (default)",
    Simulate(PomDefSOS,viewPomMerm,chor2pom) -> "Simulate Pomset",
    Visualize((p:Iterable[Pomset])=>viewSeqMerm(p,viewPomMerm), (c:Choreo) => proj.PomDefProj.allProj(chor2pom(c))) -> "Visualize projections of Pomsets"
    //...
  )

  val smallWidgets: Iterable[(Widget[Choreo],String)] = List(
    Compare((a:Choreo,b:Network[Choreo])=>
      Bisimulation.findBisimPP[Choreo,Network[Choreo]](a,b)
        (using ChorDefSOS,Network.sos(ChorDefSOS)),
      Text,
      id, c=>Network(c,ChorDefProj)
    ) -> "Default realisability (def. projection+SOS)",
    Compare((a:Choreo,b:Choreo) => // b ignored...
      SyntAnalysis.realisablePP(a),Text,id,id) -> "Experiments with syntactic realisability",
  )


  def simulateNet[S](sos:WSOS[Action,S],
                     sview:S=>Text,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
    Simulate(Network.sos(sos),net=>View.viewNetConc(net,sview),(c:Choreo)=>Network(enc(c),proj))

