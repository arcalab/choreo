package choreo.frontend

import choreo.analysis.Bisimulation
import choreo.analysis.other.SyntAnalysis
import choreo.frontend.Arcatools.Widget
import choreo.pomsets.{Choreo2Pom, PomDefSOS, Pomset}
import choreo.projection.{ChorDefProj, Projection}
import choreo.sos._
import choreo.syntax.Choreo.Action
import choreo.syntax.{Agent, Choreo}
import choreo.view._
import choreo.{sos, projection => proj}

trait Arcatools[Stx]:
  val name: String
  type T = Stx
  val parser: String=>Stx
  val widgets: Iterable[(Widget[Stx],String)]
  val smallWidgets: Iterable[(Widget[Stx],String)]

object Arcatools:
//  type GetView[A] = A => View
//  type Transform[From,To] = From=>To
  sealed trait Widget[Stx]
  case class Visualize[Stx,S](v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
  case class Simulate[Stx,A,S](sos:SOS[A,S],v:S=>View,pre:Stx=>S)
    extends Widget[Stx]
  case class Compare[Stx,R,S1,S2](comp:(S1,S2)=>R, v:R=>View, pre1:Stx=>S1, pre2:Stx=>S2)
    extends Widget[Stx]

//  def branchBisim[Stx](S)
//  case class Project[Stx,S](p:Projection[_,S],v:View[S,_],pre:Stx=>S)            extends Widget[Stx]
//  case class Realisability[Stx,G](real:(G,Projection[_,G],SOS[Action,G]) => String, pre:Stx=>G)
//                                                                                extends Widget[Stx]

  // constructors for no pre-processing
  object Visualize:
    def apply[Stx](v:Stx=>View): Visualize[Stx,Stx] = Visualize(v,c=>c)
  object Simulate:
    def apply[Stx,A](sos:SOS[A,Stx],v:Stx=>View): Simulate[Stx,A,Stx] = Simulate(sos,v,c=>c)


//  def project[Stx,S](p:Projection[_,S],v:View[Set[S],_],pre:Stx=>S): Visualize[Stx,Set[S]] =
//    Visualize(v, stx => p.allProj(pre(stx)))