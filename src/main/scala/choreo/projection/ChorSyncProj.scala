package choreo.projection

import choreo.common.Simplify
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Choreo}

/** Current attempt do project choreo expressions, adding tau-actions to capture choices made by external agents. */
object ChorSyncProj extends Projection[Agent,Choreo]:
  def getElements(c: Choreo): Set[Agent] = agents(c)
  
  def proj(c:Choreo, a:Agent): Choreo = Simplify(projAux(c,a))
  
  //todo: check DChoice
  private def projAux(c:Choreo, a:Agent): Choreo = c match
    case Send(as, bs, m) =>
      val as2 = as.filter(_==a)
      val bs2 = bs.filter(_==a)
      if as2.isEmpty && bs2.isEmpty then End
      else Send(as2,bs2,m)
    case Seq(c1, c2) => projAux(c1,a) > projAux(c2,a)
    case Par(c1, c2) => projAux(c1,a) || projAux(c2,a)
    case Choice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
      case (End,End) => End + End
      case (c1p,End) => c1p + End
      case (End,c2p) => End + c2p
      case (c1p,c2p) => c1p + c2p
    case DChoice(c1, c2) => (Simplify(projAux(c1,a)),Simplify(projAux(c2,a))) match
      case (End,End) => End + End
      case (c1p,End) => c1p + End
      case (End,c2p) => End + c2p
      case (c1p,c2p) => c1p + c2p
    case Loop(c2) => Loop(projAux(c2,a))
    case End => End
    case Tau => Tau
    case In(`a`,_,_) => c
    case Out(`a`,_,_) => c
    case Internal(`a`,_) => c
    case _:In | _:Out | _:Internal => End //Tau

