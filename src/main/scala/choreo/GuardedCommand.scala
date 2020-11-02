package choreo

import choreo.Agent._
import choreo.GuardedCommand._

import scala.util.parsing.input.Positional

/**
 * Created by guillecledou on 28/10/2020
 */


case class GuardedCommand(guards:List[Guard],commands:List[Command])


object GuardedCommand {

  sealed trait Guard

  final case class Get(agent:Agent) extends Guard
  final case class Und(agent:Agent) extends Guard

  case class Command(receiver:Agent,senders:Set[Agent])
}