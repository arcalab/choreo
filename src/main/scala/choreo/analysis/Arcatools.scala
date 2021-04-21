package choreo.analysis

import choreo.projection.Projection
import choreo.sos.{Global, Network, SOS}
import choreo.syntax.Choreo
import Choreo.Action
import choreo.view.{ChoreoMermaid, ChoreoText, LocalText, View}
import choreo.sos

object Arcatools:
  val parser: String=>Choreo = choreo.DSL.parse

  def realisable[E,G](g:G, p:Projection[E,G], sos:SOS[Action,G]): String =
    type L = Network[G]
    val l:L = Network(p.allProj(g))
    val ls:SOS[Action,L] = Network.sos(sos)
    Bisimulation.pp(Bisimulation.findBisim[G,L](g,l)(using sos,ls))

  val widgets: List[(Widget,String)] = List(
    Visualize(ChoreoMermaid) -> "Sequence Diagram",
    Simulate(Global,ChoreoText) -> "Simulate Choreo (Global)",
    Simulate(Network.sos(Global),LocalText) -> "Simulate Choreo projetions (default)"
    //...
  )
  
  sealed trait Widget
  case class Visualize[S](v:View[S])                              extends Widget
  case class Simulate[A,S](sos:SOS[A,S],v:View[S])                extends Widget
  case class Project[E,S](p:Projection[E,S],v:View[S])            extends Widget
  case class Realisability[E,A,S](p:Projection[E,S],sos:SOS[A,S]) extends Widget
