package choreo.analysis

import choreo.analysis.other.SyntAnalysis
import choreo.pomsets.{Choreo2Pom, GlobalPom, Pomset}
import choreo.projection.{ChorDefProj, Projection}
import choreo.{projection => proj}
import choreo.sos
import choreo.sos._
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.Action
import choreo.view._

object Arcatools:
  /** Parser for Choreo expressions. */
  val parser: String=>Choreo = choreo.DSL.parse

  private def id(c:Choreo):Choreo = c
  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2net[A](p:Projection[A,Choreo])(c:Choreo):Network[Choreo] =
    Network(c,p)

  val widgets: List[(Widget,String)] = List(
    Visualize(ChorMerView,id) -> "Sequence Diagram",
    Simulate(ChorDefSOS,ChorTxtView,id) -> "Simulate Choreo (Default)",
    Simulate(ChorBasicSOS,ChorTxtView,id) -> "Simulate Choreo (Basic)",
    Simulate(ChorManyTausSOS,ChorTxtView,id) -> "Simulate Choreo (ManyTaus)",
    simulateNet(ChorDefSOS,ChorTxtView,ChorDefProj,id) -> "Simulate Choreo Network (default)",
    Simulate(GlobalPom,PomMerView,chor2pom) -> "Simulate Pomset",
    Visualize(CollView(PomMerView), c => proj.PomDefProj.allProj(chor2pom(c))) -> "Visualize projections of Pomsets"
    //...
  )

//  Simulate(Network.sos(ChorDefSOS),NetwConcView(ChorTxtView),chor2net(proj.ChorDefProj)) ->
//    "Simulate Choreo projetions (default)",
//  Simulate(Network.sos(ChorManyTausSOS),NetwConcView(ChorTxtView),chor2net(proj.ChorManyTausProj)) ->
//    "Simulate Choreo projetions (many-taus)",

  val leftWidgets: List[(Widget,String)] = List(
//    Realisability(proj.ChorDefProj,ChorDefSOS,ChorTxtView,NetwConcView(ChorTxtView),id) ->
//      "Default realisability"
    Realisability(realisableDef,id) -> "Default realisability",
    Realisability[Choreo]((c:Choreo, _, _)=>SyntAnalysis.realisablePP(c),id) -> "Experiments with syntactic realisability"
  )

  /** Describes if a projectable term is realisable. */
  def realisableDef[E,G](g:G, p:Projection[E,G], sos:SOS[Action,G]): String =
    type L = Network[G]
    val l:L = Network(p.allProj(g))
    val ls:SOS[Action,L] = Network.sos(sos)
    Bisimulation.pp(Bisimulation.findBisim[G,L](g,l)(using sos,ls))

  def simulateNet[S,T<:ViewTarget](
             sos:SOS[Action,S],view:View[S,T],proj:Projection[_,S],enc:(Choreo=>S)): Simulate[Action,Network[S]] =
    Simulate(Network.sos(sos),NetwConcView[S,T](view),(c:Choreo)=>Network(enc(c),proj))

  sealed trait Widget
  case class Visualize[S](v:View[S,_],pre:Choreo=>S)                            extends Widget
  case class Simulate[A,S](sos:SOS[A,S],v:View[S,_],pre:Choreo=>S)              extends Widget
  case class Project[E,S](p:Projection[E,S],v:View[S,_],pre:Choreo=>S)          extends Widget
  case class Realisability[G](real:(G,Projection[_,G],SOS[Action,G]) => String, pre:Choreo=>G)
                                                                                extends Widget
//  case class Realisability[E,A,S](p:Projection[E,S],
//                                  sos:SOS[A,S],
//                                  gv:View[S,Text],
//                                  lv:View[Network[S],Text],
//                                  pre:Choreo=>S) extends Widget
