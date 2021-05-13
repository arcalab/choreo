package choreo

import choreo.analysis.other.SyntAnalysis
import choreo.view.DotPomsets
import choreo.common.ParsingException
import choreo.syntax.Choreo._
import choreo.syntax._
import choreo.sos.{Network,ChorDefSOS}

import scala.language.implicitConversions
import choreo.view.DotPomsets.dotPomset
import choreo.analysis.{_}
import choreo.pomsets._
import caos.sos.BranchBisim
import caos.sos.BranchBisim._


object DSL :
  implicit def str2agent(s:String):Agent = Agent(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))

  val end: Choreo = End
  val tau: Choreo = Tau
  def loop(e:Choreo): Loop = Loop(e)
  
  def realisablePP(c:Choreo) = SyntAnalysis.realisablePP(c)
  def realisable(c:Choreo) = SyntAnalysis.realisable(c)
  def findBisimDef(c:Choreo): BranchBisim.BResult[Action,Choreo,Network[Choreo]] =
    val l = Network(projection.ChorDefProj.allProj(c))
    if Bounded.boundedChoreo(c)
    then  findBisim(c,l)(using ChorDefSOS,Network.sos(ChorDefSOS))
    else Left(BranchBisim.BEvid(Set(List("Found an unbounded loop.")),Set(),0))

  def findBisimDefPP(c:Choreo) = println(BranchBisim.pp(findBisimDef(c)))
//  def findBisim1(c:Choreo) = Bisimulation.findBisimBasic(c)
//  def findBisim2(c:Choreo) = Bisimulation.findBisimManyTaus(c)
  def comGraphsPP(c:Choreo) = ComGraph.comGraphsPP(c)
  def pomset(c:Choreo): Pomset = Choreo2Pom(c)


  def parse(choreo: String): Choreo = Parser.parse(choreo) match
    case Parser.Success(res, _) => res
    case f: Parser.NoSuccess => throw new ParsingException("Parser failed: " + f)
  
  
