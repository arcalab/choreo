package choreo.oldChoreo.syntax

import choreo.oldChoreo.syntax.Program.{ChannelDef, ChoreographyExp}

import scala.util.parsing.input.Positional

/**
 * Created by guillecledou on 28/10/2020
 */


case class Program(channels:List[ChannelDef],choreography:ChoreographyExp)

object Program {

  final case class ChannelDef(name:String,
                        memories:List[Variable],senders:List[Variable],receivers:List[Variable],
                        gcs:List[GuardedCommandDef]) extends Positional

  final case class GuardedCommandDef(guard:List[GuardDef], command:List[CommandDef]) extends Positional

  sealed trait GuardDef extends Positional
  final case class GetExp(names:List[Variable]) extends GuardDef
  final case class UndExp(names:List[Variable]) extends GuardDef

  final case class Variable(name:String) extends Positional

  final case class CommandDef(lhs:Variable,rhs:Set[Variable]) extends Positional

  sealed trait ChoreographyExp extends Positional
  final case class InteractionExp(senders:List[Variable], receivers:List[Variable],
                               memories:List[Variable], channelName:String)       extends ChoreographyExp
  final case class SeqExp(c1: ChoreographyExp, c2: ChoreographyExp)             extends ChoreographyExp
  final case class ChoiceExp(c1: ChoreographyExp, c2: ChoreographyExp)          extends ChoreographyExp
  final case class ParExp(c1: ChoreographyExp, c2: ChoreographyExp)             extends ChoreographyExp
  final case class LoopExp(c: ChoreographyExp)                                  extends ChoreographyExp

}