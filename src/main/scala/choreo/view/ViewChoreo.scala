package choreo.view

import choreo.syntax.Choreo
import choreo.pomsets.Pomset
import choreo.sos.{Network}
import Choreo.Action
import mat.view.View
import mat.view._

/**
 * A `View` is an object that, given an argument, can produce a string that represents it.
 * We consider 2 kinds of views: for textual representation and for diagrams, written
 * using `mermaid` notation.
 *
 * @tparam A is the type of the argument that can be viewed as a String.
 */
//abstract class View[-A](target:ViewTarget):
//  def view(a:A):String
////  val target: ViewTarget
//
//trait View2(code:String, tarteg:ViewTarget) {
//  val code2:String
//
//}
//sealed trait View(code:String)
//
//case class Mermaid(code:String) extends View(code)
//case class Text(code:String)    extends View(code)
//case class Html(code:String)    extends View(code)

//trait MermaidView[-A] extends View[A]
//trait TextView[-A] extends View[A]
//sealed abstract class ViewTarget // only used by the type system
//object Mermaid extends ViewTarget
//object Text    extends ViewTarget
//object Html    extends ViewTarget


////////////////////
// Existing views //
////////////////////
object ViewChoreo:
  def viewChorMerm(c:Choreo) = Mermaid(SequenceChart(c))
  def viewChorTxt(c:Choreo)  = Text(SequenceChart(c))
  def viewPomTxt(c:Choreo)  = Text(c.toString)

  def viewPomMerm(p:Pomset) = Mermaid(MermaidPomset(p))
  def viewNetConc[S](c:Network[S],sview:S=>Text): Text =
    c.proj.map((l:S)=>sview(l)).fold(Text(""))((a,b)=>Text(a.code+b.code))
  def viewSeq[S](cs:Iterable[S],sview:S=>Text) =
    cs.map((l:S)=>sview(l)).fold(Text(""))((a,b)=>Text(a.code+b.code))
  def viewSeqMerm[S](cs:Iterable[S],sview:S=>Mermaid) =
    cs.map((l:S)=>sview(l)).fold(Mermaid(""))((a,b)=>Mermaid(a.code+b.code))


//object ChorMerView extends View[Choreo,Mermaid]: // Mermaid.type if object
//  def view(c:Choreo) = SequenceChart(c)
//
//object ChorTxtView extends View[Choreo,Text]:
//  def view(c:Choreo) = c.toString
//
//object PomTxtView extends View[Pomset,Text]:
//  def view(p:Pomset) = p.toString
//
//object PomMerView extends View[Pomset,Mermaid]:
//  def view(p:Pomset) = MermaidPomset(p)
//
//
//case class NetwConcView[S,T<:ViewTarget](view:View[S,T]) extends View[Network[S],T]:
//  def view(c:Network[S]) = c.proj.map((l:S)=>view.view(l)).fold("")(_+_)
//
//case class SeqView[S,T<:ViewTarget](view:View[S,T]) extends View[Iterable[S],T]:
//  def view(cs:Iterable[S]) = cs.map((l:S)=>view.view(l)).fold("")(_+_)
