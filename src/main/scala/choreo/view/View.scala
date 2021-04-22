package choreo.view

import choreo.syntax.Choreo
import choreo.pomsets.Pomset
import choreo.sos.{Network, SOS}
import Choreo.Action

/**
 * A `View` is an object that, given an argument, can produce a string that represents it.
 * We consider 2 kinds of views: for textual representation and for diagrams, written
 * using `mermaid` notation.
 *
 * @tparam A is the type of the argument that can be viewed as a String.
 */
abstract class View[-A,T<:ViewTarget]:
  def view(a:A):String
//  val target: ViewTarget

//trait MermaidView[-A] extends View[A]
//trait TextView[-A] extends View[A]
sealed abstract class ViewTarget // only used by the type system
case class Mermaid() extends ViewTarget
case class Text()    extends ViewTarget


////////////////////
// Existing views //
////////////////////

object ChorMerView extends View[Choreo,Mermaid]: // Mermaid.type if object
  def view(c:Choreo) = SequenceChart(c)

object ChorTxtView extends View[Choreo,Text]:
  def view(c:Choreo) = c.toString

object PomTxtView extends View[Pomset,Text]:
  def view(p:Pomset) = p.toString

object PomMerView extends View[Pomset,Mermaid]:
  def view(p:Pomset) = MermaidPomset(p)


case class NetwConcView[S,T<:ViewTarget](view:View[S,T]) extends View[Network[S],T]:
  def view(c:Network[S]) = c.proj.map((l:S)=>view.view(l)).fold("")(_+_)

case class CollView[S,T<:ViewTarget](view:View[S,T]) extends View[Iterable[S],T]:
  def view(cs:Iterable[S]) = cs.map((l:S)=>view.view(l)).fold("")(_+_)
