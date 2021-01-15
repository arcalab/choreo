package choreo.choreo2

import choreo.choreo2.analysis.Sprints
import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._

import scala.language.implicitConversions
import choreo.choreo2.analysis._

object DSL :
  implicit def str2agent(s:String):Agent = Agent(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))
  
  val end: Choreo = End
  def loop(e:Choreo): Loop = Loop(e)
  
  def realisablePP(c:Choreo) = Sprints.realisablePP(c)
  def findBisimPP(c:Choreo) = Bisimulation.findBisimPP(c)
  def comGraphsPP(c:Choreo) = ComGraph.comGraphsPP(c)
  
  
