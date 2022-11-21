package choreo.projection

import choreo.common.Simplify
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

//////////////////////////////////////////////////
// Earlier attempts, with different projections //
//////////////////////////////////////////////////

/** Early attempt do project choreo expressions, replacing external actions with "end" */
object ChorBasicProj extends Projection[Agent,Choreo]:
  def getElements(c: Choreo): Set[Agent] = agents(c)
  
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projAux(c,a))
  
  //todo: check DChoice
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      //      val outs = as.filter(_==a).flatMap(a2=>bs.map(b=>a2!b by m))
      //      val ins  = bs.filter(_==a).flatMap(b=>as.map(a2=>b?a2 by m))
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projAux((outs++ins).fold(End)(_>_) , a)
    case Seq(c1, c2) => projAux(c1,a) > projAux(c2,a)
    case Par(c1, c2) => projAux(c1,a) || projAux(c2,a)
    case Choice(c1, c2) =>projAux(c1,a) + projAux(c2,a)
    case DChoice(c1, c2) =>projAux(c1,a) + projAux(c2,a)
    case Loop(c2) => Loop(projAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case Internal(`a`,_) => c
    case _:In | _:Out | _:Internal => End //Tau
