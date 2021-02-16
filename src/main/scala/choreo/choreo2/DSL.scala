package choreo.choreo2

import choreo.choreo2.common.ParsingException

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
  def findBisim(c:Choreo) = Bisimulation.findBisim(c)
  def findBisimPP(c:Choreo) = println(Bisimulation.pp(findBisim(c)))
//  def findBisim1(c:Choreo) = Bisimulation.findBisimBasic(c)
//  def findBisim2(c:Choreo) = Bisimulation.findBisimManyTaus(c)
  def comGraphsPP(c:Choreo) = ComGraph.comGraphsPP(c)
  def pomset(c:Choreo): Pomset = ChoreoPom(c)


  def parse(choreo: String): Choreo = Parser.parse(choreo) match
    case Parser.Success(res, _) => res
    case f: Parser.NoSuccess => throw new ParsingException("Parser failed: " + f)
  
  
