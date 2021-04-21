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
sealed abstract class View[-A]:
  def view(a:A):String

trait MermaidView[-A] extends View[A]
trait TextView[-A] extends View[A]

////////////////////
// Existing views //
////////////////////

object ChoreoMermaid extends MermaidView[Choreo]:
  def view(c:Choreo) = SequenceChart(c)

object ChoreoText extends TextView[Choreo]:
  def view(c:Choreo) = c.toString

//class LocalText[S](view:View[S]) extends TextView[LocalSOS[S]]:
//  def view(c:LocalSOS[S]) =  c.toString

object LocalText extends TextView[Network[_]]:
  def view(c:Network[_]) = c.toString

object PomsetText extends TextView[Pomset]:
  def view(p:Pomset) = p.toString

object PomsetMermaid extends MermaidView[Pomset]:
  def view(p:Pomset) = MermaidPomset(p)
