package choreo.choreo2.backend

import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo._
import scala.annotation.tailrec

object Simplify: 
  
  /** Simple heuristic to apply some simplifications. */
  private def simpleOnce(c: Choreo): Choreo = c match
    case Seq(End, c2) => simpleOnce(c2)
    case Seq(c1, End) => simpleOnce(c1)
    case Par(End, c2) => simpleOnce(c2)
    case Par(c1, End) => simpleOnce(c1)
    case Seq(c1, c2) => simpleOnce(c1) >  simpleOnce(c2)
    case Par(c1, c2) => simpleOnce(c1) || simpleOnce(c2)
    case Choice(c1, c2) if c1==c2 => simpleOnce(c1)
    case Choice(c1, c2) => simpleOnce(c1) + simpleOnce(c2)
    case Loop(End) => End
    case Loop(c2) => Loop(simpleOnce(c2))
    case End | _:Send | _:Action => c
  
  @tailrec
  def simple(c: Choreo): Choreo =
    val c2 = simpleOnce(c)
    if c2==c then c else simple(c2)