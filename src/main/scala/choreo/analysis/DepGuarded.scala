package choreo.analysis

import choreo.sos.ChorDefSOS
import choreo.syntax.{Choreo, Msg}
import choreo.syntax.Choreo.*

object DepGuarded:
  type Result[A] = Either[List[String],A]
  /** Checks if a choreography is dependently guarded. */
  def apply(c:Choreo): Result[Unit] = dependentlyGuarded(c)

  def dependentlyGuarded(c: Choreo): Result[Unit] = c match
    case Seq(c1, c2) => join(dependentlyGuarded(c1), dependentlyGuarded(c2))
    case Par(c1, c2) => join(dependentlyGuarded(c1), dependentlyGuarded(c2))
    case Choice(c1, c2) => join(dependentlyGuarded(c1), dependentlyGuarded(c2))
    case DChoice(c1, c2) => join(dependentlyGuarded(c1), dependentlyGuarded(c2))
    case Loop(c) => join(checkDependency(c),dependentlyGuarded(c))
    case _ => Right(())

  /** Check if c --agree(l)--> c' implies that c==c', for any action (that involves c). */
  def checkDependency(c: Choreo): Either[List[String],Unit] =
    for a<-agents(c) do
      val ell = Out(a,a,Msg(Nil))
      ChorDefSOS.agrees(c, ell) match
        case Some(c2) if c!=c2 => return Left(List(s"'${c}' partially evaluates to '${c2}' for agent $a, and they differ."))
        case _ =>
    Right(())

  def join[A, B](e1: Either[List[A], B], e2: Either[List[A], B]): Either[List[A], B] = (e1, e2) match
    case (Right(_), _) => e2
    case (Left(l1), Left(l2)) => Left(l1 ::: l2)
    case _ => e1