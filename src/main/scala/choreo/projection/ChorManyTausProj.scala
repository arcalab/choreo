package choreo.projection

import choreo.common.Simplify
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo}

object ChorManyTausProj extends Projection[Agent,Choreo]:
  def getElements(c: Choreo): Set[Agent] = agents(c)
  
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projTauAux(c,a))
  
  //todo: check DChoice
  private def projTauAux(c:Choreo, a:Agent): Choreo = c match
    case Send(List(`a`), List(b), m) => Out(a,b,m)
    case Send(List(b), List(`a`), m) => In(a,b,m)
    case Send(List(_), List(_), _) => Tau
    case Send(as, bs, m) =>
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projTauAux((outs++ins).fold(End)(_>_) , a)

    case Seq(c1, c2) => projTauAux(c1,a) > projTauAux(c2,a)
    case Par(c1, c2) => projTauAux(c1,a) || projTauAux(c2,a)
    case Choice(c1, c2) =>projTauAux(c1,a) + projTauAux(c2,a)
    case DChoice(c1, c2) =>projTauAux(c1,a) + projTauAux(c2,a)//todo: check DChoice
    case Loop(c2) => Loop(projTauAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c // never user
    case Out(`a`,_,_) => c // never used
    case _:In | _:Out => Tau // never used


