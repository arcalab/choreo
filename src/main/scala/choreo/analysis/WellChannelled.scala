package choreo.analysis

import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.*

/**
 * 2. Single/causal channels
 * - forall e1: ab!?x, e2: ab!?y
 *   - e1<=e2 OR
 *   - e2<=e1 OR
 *   - e1 in C1, e2 in C2, C1+C2
 *
 * - Equivalent in Choreo (?)
 *   - C1 || C2 => channels(C1) # channels(C2)?
 */
object WellChannelled:

  import WellBranched.Result
  import WellBranched.&&

  def apply(c:Choreo): Result = c match
    case Par(c1, c2) => wellChannelled(c1,c2)
    //
    case Seq(c1, c2) => apply(c1) && apply(c2)
    case Choice(c1, c2) => apply(c1) && apply(c2)
    case DChoice(c1, c2) => apply(Choice(c1,c2))
    case Loop(c) => apply(c)
    case _ => Right(())

  def wellChannelled(c1: Choreo, c2: Choreo): Result =
    val (ch1,ch2) = (channels(c1),channels(c2))
    if ch1.intersect(ch2).nonEmpty
    then Left(s"Common channels [${ch1.intersect(ch2).mkString(",")}] found in parallel: $c1 || $c2")
    else Right(())

  /** Returns the set of all active agents */
  def channels(c:Choreo): Set[(Agent,Agent)] = c match
    case Send(as, bs, _) => for a<-as.toSet; b<-bs.toSet yield (a,b)
    case Seq(c1, c2) => channels(c1) ++ channels(c2)
    case Par(c1, c2) => channels(c1) ++ channels(c2)
    case Choice(c1, c2) => channels(c1) ++ channels(c2)
    case DChoice(c1, c2) => channels(c1) ++ channels(c2)
    case Loop(c) => channels(c)
    case End => Set()
    case Tau => Set()
    case In(b, a, _)  => Set((a,b))
    case Out(a, b, _) => Set((a,b))