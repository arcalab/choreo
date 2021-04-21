package choreo.projection

import choreo.common.Simplify
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.{Choice, DChoice, End, In, Loop, Out, Par, Send, Seq, Tau, agents}

object Default extends Projection[Agent,Choreo]:
  def getElements(c: Choreo): Set[Agent] = agents(c)
  
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projAux(c,a))
  
  //todo: check DChoice
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      val outs = as.flatMap(a2=>bs.map(b=>a2!b by m))
      val ins  = bs.flatMap(b=>as.map(a2=>b?a2 by m))
      projAux((outs++ins).fold(End)(_>_) , a)
    case Seq(c1, c2) => projAux(c1,a) > projAux(c2,a)
    case Par(c1, c2) => projAux(c1,a) || projAux(c2,a)
    case Choice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
      case (End,End) => Tau + Tau
      case (c1p,End) => c1p + Tau
      case (End,c2p) => Tau + c2p
      case (c1p,c2p) => c1p + c2p
    case DChoice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
      case (End,End) => Tau + Tau
      case (c1p,End) => c1p + Tau
      case (End,c2p) => Tau + c2p
      case (c1p,c2p) => c1p + c2p
    case Loop(c2) => Loop(projAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case _:In | _:Out => End

