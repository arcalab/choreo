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
    Visualize(ChorMerView,id) -> "Sequence Diagram",
    Simulate(ChorDefSOS,ChorTxtView,id) -> "Simulate Choreo (Default)",
    Simulate(ChorBasicSOS,ChorTxtView,id) -> "Simulate Choreo (Basic)",
    Simulate(ChorManyTausSOS,ChorTxtView,id) -> "Simulate Choreo (ManyTaus)",
    simulateNet(ChorDefSOS,ChorTxtView,ChorDefProj,id) -> "Simulate Choreo Network (default)",
    Simulate(PomDefSOS,PomMerView,chor2pom) -> "Simulate Pomset",
    Visualize(SeqView(PomMerView), (c:Choreo) => proj.PomDefProj.allProj(chor2pom(c))) -> "Visualize projections of Pomsets"
    //...
  )

  val smallWidgets: Iterable[(Widget[Choreo],String)] = List(
    Compare((a:Choreo,b:Network[Choreo])=>
      Bisimulation.findBisimPP[Choreo,Network[Choreo]](a,b)
        (using ChorDefSOS,Network.sos(ChorDefSOS)),
      id, c=>Network(c,ChorDefProj)
    ) -> "Default realisability (def. projection+SOS)",
    Compare((a:Choreo,b:Choreo) => // b ignored...
      SyntAnalysis.realisablePP(a),id,id) -> "Experiments with syntactic realisability",
  )


  def simulateNet[S,T<:ViewTarget](sos:SOS[Action,S],
                                   view:View[S,T],
                                   proj:Projection[_,S],
                                   enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
    Simulate(Network.sos(sos),NetwConcView[S,T](view),(c:Choreo)=>Network(enc(c),proj))

