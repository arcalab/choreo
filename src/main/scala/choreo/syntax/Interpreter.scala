package choreo.syntax

import cats.data.StateT
import cats.implicits._
import choreo.Agent.{Memory, Participant}
import choreo.Choreography._
import choreo.GuardedCommand._
import choreo.syntax.GlobalContext._
import choreo.syntax.Program._
import choreo._

import scala.util.parsing.input.{Position, Positional}

/**
 * Created by guillecledou on 29/10/2020
 */

object Interpreter {
  type ErrorOr[A] = Either[InterpreterError, A]
  type Interpret[A] = StateT[ErrorOr, GlobalContext, A]

  sealed trait InterpreterError extends Positional

  final case class DefinitionError(msg: String) extends InterpreterError

  final case class DeclarationError(err: ContextError) extends InterpreterError

  def apply(program: Program): ErrorOr[(Choreography, Ctx[Channel])] = for {
    (gc, choreo) <- interpret(program).run(GlobalContext())
    channels = gc.channels.context.map(e=> e._1->e._2.channel)
  } yield (choreo, channels)

  def interpret(p: Program): Interpret[Choreography] = for {
    _ <- p.channels.traverse(interpret)
    choreo <- interpret(p.choreography)
  } yield choreo


  def getSt: Interpret[GlobalContext] =
    StateT.get

  def setSt(gc: GlobalContext): Interpret[Unit] =
    StateT.set(gc)

  def clearSt: Interpret[GlobalContext] = for {
    st <- getSt
    nst = st.newScope()
    _ <- setSt(nst)
  } yield nst

  def liftError[A](errorOr: ErrorOr[A]): Interpret[A] = errorOr.fold(
    err => StateT.liftF[ErrorOr, GlobalContext, A](Either.left(err)),
    ok => ok.pure[Interpret]
  )

  def liftCtxError[A](errorOr: CtxErrorOr[A], pos: Position): Interpret[A] = errorOr.fold(
    err => StateT.liftF[ErrorOr, GlobalContext, A](Either.left(DeclarationError(err).setPos(pos))),
    ok => ok.pure[Interpret]
  )

  def defError[A](msg: String, pos: Position): Interpret[A] =
    liftError[A](Either.left(DefinitionError(msg).setPos(pos)))

  def interpret(ch: ChannelDef): Interpret[Channel] = for {
    memories <- ch.memories.traverse(interpretMemory)
    senders <- ch.senders.traverse(v=>interpretParticipant(v,(a:Agent)=>OutEntry(a)))
    receivers <- ch.receivers.traverse(v=>interpretParticipant(v,(a:Agent)=>InEntry(a)))
    gcs <- ch.gcs.traverse(interpret)
    channel = Channel(ch.name, senders, receivers, memories, gcs)
    _ <- isClosed(channel,ch.pos)
    _ <- addChannel(channel,ch.pos)
    _ <- clearSt
  } yield channel

  def isClosed(ch:Channel,pos:Position):Interpret[Unit] = for {
    st <- getSt
    notUsed = st.agents.context.values.map(_.agent).toSet -- (ch.senders++ch.receivers++ch.memories).toSet
    _ <- if (notUsed.isEmpty) ().pure[Interpret]
    else defError(s"Agents declared but not used: ${notUsed}",pos)
  } yield ()

  def addChannel(ch: Channel,pos:Position): Interpret[Unit] = for {
    st <- getSt
    nst <- liftCtxError[GlobalContext](st.addChannel(ch.name, CEntry(ch).setPos(pos)), pos)
    _ <- setSt(nst)
  } yield ()

  def addAgentEntry(e: AEntry): Interpret[Unit] = for {
    st <- getSt
    nst <- liftCtxError[GlobalContext](st.addAgent(e.agent.name, e.setPos(e.pos)), e.pos)
    _ <- setSt(nst)
  } yield ()

  def interpretMemory(v: Variable): Interpret[Memory] = for {
    st <- getSt
    memory = Memory(v.name)
    _ <- addAgentEntry(MemEntry(memory).setPos(v.pos))
  } yield memory

  def interpretParticipant(v: Variable,mkEntry:Agent=>AEntry): Interpret[Participant] = for {
    st <- getSt
    participant = Participant(v.name)
    _ <- addAgentEntry(mkEntry(participant).setPos(v.pos))
  } yield participant

  def interpret(gcd: GuardedCommandDef): Interpret[GuardedCommand] = for {
    g <- gcd.guard.traverse(interpret)
    c <- gcd.command.traverse(interpret)
    gc = GuardedCommand(g.flatten, c)//.setPos(gcd.pos)
    _ <- checkWellDefinedGC(gc,gcd.pos)
  } yield gc

  // todo: check assigned only once
  def checkWellDefinedGC(gc: GuardedCommand,pos:Position): Interpret[Unit] = {
    val senders = gc.commands.flatMap(c => c.senders).toSet
    val gets = gc.guards.collect({ case g: Get => g.agent }).toSet
    if (senders.subsetOf(gets)) ().pure[Interpret]
    else defError(s"Agents ${senders.diff(gets).map(_.name).mkString(",")} must be gotten", pos)
  }

  def interpret(guard: GuardDef): Interpret[List[Guard]] = guard match {
    case GetExp(names) => for {
      agents <- names.traverse(getAgent)
      gets = agents.map(a => Get(a))//.setPos(a.pos))
    } yield gets
    case UndExp(names) => for {
      agents <- names.traverse(getAgent)
      unds = agents.map(a => Und(a))//.setPos(a.pos))
    } yield unds
  }

  def interpret(cmd: CommandDef): Interpret[Command] = for {
    lhs <- getAgent(cmd.lhs)
    rhs <- cmd.rhs.toList.traverse(getAgent)
  } yield Command(lhs, rhs.toSet)//.setPos(cmd.pos)

  def getAgent(v: Variable): Interpret[Agent] = for {
    st <- getSt
    agent <- liftCtxError(st.getAgent(v.name), v.pos)
  } yield agent

  def canMatch(i: Interaction, ch: Channel,pos:Position): Interpret[Unit] =
    if ((i.senders.size != ch.senders.size) ||
      (i.receivers.size != ch.receivers.size) ||
      (i.memories.size > ch.memories.size))
      defError(s"Cannot match interaction with channel ${ch.name} signature", pos)
    else ().pure[Interpret]

//  def checkLoopCom(senders:List[Agent],receivers:List[Agent],pos:Position):Interpret[Unit] =
//    if (senders.intersect(receivers).nonEmpty)
//      defError[Unit](s"Participants (${senders.intersect(receivers).mkString(",")}) " +
//        s"trying to send messages to themselves", pos)
//    else ().pure[Interpret]

  def interpret(c: ChoreographyExp): Interpret[Choreography] = c match {
    case InteractionExp(snds, rcvs, mems, chName) => {
      val memories = mems.map(v => Memory(v.name))
      val senders = snds.map(v => Participant(v.name))
      val receivers = rcvs.map(v => Participant(v.name))
      for {
        st <- getSt
        //_ <- checkLoopCom(senders,receivers,c.pos)
        channel <- liftCtxError[Channel](st.getChannel(chName), c.pos)
        interaction = Interaction(senders, receivers, memories, chName)//.setPos(c.pos)
        _ <- clearSt
        _ <- canMatch(interaction, channel,c.pos)
      } yield interaction
    }
    case SeqExp(c1, c2) => for {
      cc1 <- interpret(c1)
      cc2 <- interpret(c2)
    } yield Seq(cc1, cc2)//.setPos(c.pos)
    case ChoiceExp(c1, c2) => for {
      cc1 <- interpret(c1)
      cc2 <- interpret(c2)
    } yield Choice(cc1, cc2)//.setPos(c.pos)
    case ParExp(c1, c2) => for {
      cc1 <- interpret(c1)
      cc2 <- interpret(c2)
    } yield Par(cc1, cc2)//.setPos(c.pos)
    case LoopExp(c) => for {
      cc <- interpret(c)
    } yield Loop(cc)//.setPos(c.pos)
  }


}
