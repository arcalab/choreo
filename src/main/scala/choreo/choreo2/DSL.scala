package choreo.choreo2

import scala.language.implicitConversions
import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.view.DotPomsets
//import choreo.choreo2.view.Dot._
import choreo.choreo2.view.DotPomsets.dotPomset
//import choreo.choreo2.view.DotNPom
import choreo.choreo2.analysis._
import choreo.choreo2.analysis.pomsets._
import choreo.choreo2.analysis.SyntAnalysis


object DSL :
  implicit def str2agent(s:String):Agent = Agent(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))

  val end: Choreo = End
  val tau: Choreo = Tau
  def loop(e:Choreo): Loop = Loop(e)
  
  def realisablePP(c:Choreo) = SyntAnalysis.realisablePP(c)
  def realisable(c:Choreo) = SyntAnalysis.realisable(c)
  def findBisimPP(c:Choreo) = Bisimulation.findBisimPP(c)
  def findBisim(c:Choreo) = Bisimulation.findBisim(c)
  def findWBisimPP(c:Choreo) = Bisimulation.findWBisimPP(c)
  def comGraphsPP(c:Choreo) = ComGraph.comGraphsPP(c)
  def pomset(c:Choreo): String = ChoreoPom(c).toDot

  //def npomset(c:Choreo): String = ChoreoNPom(c).toDot
  
  
