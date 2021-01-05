package choreo.backend

import choreo.syntax.{Interpreter, Program}
import choreo.syntax.Program._
import choreo.semantics.Pomset.Label
import choreo.semantics.Pomset.Label.Role
import choreo.syntax.GlobalContext.ContextError
import choreo.syntax.Interpreter.InterpreterError

/**
 * Created by guillecledou on 29/10/2020
 */

object Show:

  def apply(p: Program): String =
    p.channels.map(apply).mkString("\n\n") + "\n\n" + apply(p.choreography)

  def apply(c: ChannelDef): String =
    c.name + "<" + c.memories.map(apply).mkString(",") + ">" +
      par(c.senders.map(apply).mkString(",")) +
      par(c.receivers.map(apply).mkString(",")) + "{\n" +
      " " + c.gcs.map(apply).mkString("\n ") + "\n}"

  def apply(v: Variable): String = v.name

  def apply(gc: GuardedCommandDef): String =
    apply(gc.guard) + " -> " + gc.command.map(apply).mkString(", ")

  def apply(gs: List[GuardDef]): String =
    if gs.isEmpty then "true" else
      val gets = gs.collect { case g: GetExp => g.names.map(_.name) }.flatten
      val unds = gs.collect { case g: UndExp => g.names.map(_.name) }.flatten
      var res: List[String] = Nil
      if unds.nonEmpty then res ::= s"und(${unds.mkString(",")})"
      if gets.nonEmpty then res ::= s"get(${gets.mkString(",")})"
      res.mkString(", ")

  def apply(c: CommandDef): String =
    val rhs = c.rhs.toList
    s"${apply(c.lhs)}:=${
      if rhs.size == 1 then
        apply(rhs.head)
      else rhs.map(apply).mkString("{", ",", "}")}"

  def apply(c: ChoreographyExp): String = c match
    case InteractionExp(senders, receivers, memories, channelName) =>
      senders.map(apply).mkString(",") +
        s" --${channelName}(${memories.map(apply).mkString(",")})--> " +
        receivers.map(apply).mkString(",")
    case SeqExp(c1, c2) => par(apply(c1) + ";" + apply(c2))
    case ChoiceExp(c1, c2) => par(apply(c1) + "+" + apply(c2))
    case ParExp(c1, c2) => par(apply(c1) + "||" + apply(c2))
    case LoopExp(c) => par(apply(c)) + "*"

  def par(s: String): String = "(" + s + ")"

  def apply(label:Label):String =
    label.active.name + apply(label.role) + label.passive.map(_.name).mkString(",")

  def apply(role:Role):String = role match
    case Label.In => "?"
    case Label.Out =>"!"
    case Label.OverrideIn => "??"

  def apply(err:InterpreterError):String =
    s"[${err.pos}] ${err match {
        case Interpreter.DefinitionError(msg) => msg
        case Interpreter.DeclarationError(err) => err.msg
      }
    }"
