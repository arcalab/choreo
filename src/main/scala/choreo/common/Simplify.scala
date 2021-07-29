package choreo.common

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Events}
import choreo.syntax.Choreo
import choreo.syntax.Choreo._

import scala.annotation.tailrec

object Simplify: 
  
  /** Simple heuristic to apply some simplifications. */
  private def simpleOnce(c: Choreo): Choreo =
    //println(s"- simplifying...")
    val x = c match
      // Associate ;
      case Seq(Seq(c1,c2),c3) => simpleOnce(c1 > (c2 > c3))
      // Associate +
      case Choice(Choice(c1,c2),c3) => simpleOnce(c1 + (c2 + c3))
      case DChoice(DChoice(c1,c2),c3) => simpleOnce(c1 ++ (c2 ++ c3))
      // End-removal
      case Seq(End, c2) => simpleOnce(c2)
      case Seq(c1, End) => simpleOnce(c1)
      case Par(End, c2) => simpleOnce(c2)
      case Par(c1, End) => simpleOnce(c1)
      case Loop(End) => End
      // Tau-removal
      case Seq(Tau,Tau) => Tau
      case Seq(Tau,Seq(Tau,c2)) => Tau > simpleOnce(c2)
      case Choice(Tau,Tau) => Tau
      case DChoice(Tau,Tau) => Tau
      // Recursive calls
      case Seq(c1, c2) => simpleOnce(c1) >  simpleOnce(c2)
      case Par(c1, c2) => simpleOnce(c1) || simpleOnce(c2)
      case Choice(c1, c2) if c1==c2 => simpleOnce(c1)
      case DChoice(c1, c2) if c1==c2 => simpleOnce(c1)
      case Choice(c1, c2) => simpleOnce(c1) + simpleOnce(c2)
      case DChoice(c1, c2) => simpleOnce(c1) + simpleOnce(c2)
      case Loop(c2) => Loop(simpleOnce(c2))
      // trivial cases
      case End | Tau | _:Send | _:Action => c
    //println(s"done: $x")
    x
  
  @tailrec
  def apply(c: Choreo): Choreo =
    val c2 = simpleOnce(c)
    if c2==c then c else apply(c2)


//  def simpleOnce(nest: Events, acts: Actions): Events =
//    var newEvents: Set[Events] = Set()
//    var newChoices: Set[NPomset.NChoice[NPomset.Event]] = Set()
//    for c<-nest.choices do
//      if similar(c.left,c.right,acts) then
//        newEvents += c.left
//      else
//        newChoices += c
