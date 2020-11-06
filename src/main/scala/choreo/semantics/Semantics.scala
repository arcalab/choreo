package choreo.semantics

import cats.data.ReaderT
import cats.implicits._
import choreo.Agent.{Memory, Participant}
import choreo.Choreography.Interaction
import choreo.GuardedCommand._
import choreo._
import choreo.semantics.Pomset.Label._
import choreo.semantics.Pomset._
import choreo.syntax.GlobalContext.{Context, Ctx}

import scala.util.parsing.input.Positional


/**
 * Created by guillecledou on 31/10/2020
 */


object Semantics {

  private var seedId = 0
  private def seed():Int = {seedId+=1;seedId-1}

  private var memId = 0
  private def freshMem():String = {memId+=1;s"_m${memId-1}"}

  def apply(c:Choreography)(implicit channels:Ctx[Channel]):PomsetFamily = c match {
    case i@Choreography.Interaction(senders, receivers, memories, name) =>
      val ch = channels(name)
      implicit val replace:Map[Agent,Agent] = buildReplacement(ch,i)
      val nch = Channel(name,senders,receivers,memories,ch.guardedCommands.map(instantiate))
      apply(nch)
    case Choreography.Seq(c1, c2) => apply(c1) >> apply(c2)
    case Choreography.Choice(c1, c2) => apply(c1) + apply(c2)
    case Choreography.Par(c1, c2) => apply(c1) || apply(c2)
    case Choreography.Loop(c) => apply(c)^1
  }

  private def buildReplacement(ch:Channel,i: Interaction):Map[Agent,Agent] = {
    val iMemories = i.memories++ List.fill(ch.memories.size - i.memories.size)(Memory(freshMem()))
    (ch.senders.zip(i.senders) ++ ch.receivers.zip(i.receivers) ++ ch.memories.zip(iMemories)).toMap
  }

  private def instantiate(gc:GuardedCommand)(implicit map:Map[Agent,Agent]):GuardedCommand =
    GuardedCommand(gc.guards.map(instantiate), gc.commands.map(instantiate))

  private def instantiate(c:Command)(implicit map:Map[Agent,Agent]):Command =
    Command(map(c.receiver),c.senders.map(map))

  private def instantiate(g:Guard)(implicit map:Map[Agent,Agent]):Guard = g match {
    case Get(agent) => Get(map(agent))
    case Und(agent) => Und(map(agent))
  }

  private def apply(ch:Channel):PomsetFamily =
    ch.guardedCommands.map(gc=>lift(apply(gc)))
      .foldRight(PomsetFamily.identity)(_ compose _)

  private def lift(p:Pomset):PomsetFamily =
    PomsetFamily(Set(p))

  private def apply(gc:GuardedCommand):Pomset = {
    val lbs = labels(gc)
    val orders = order(gc,lbs)
    Pomset(lbs.keySet,lbs,orders)
  }

  private def lhs(cs:List[Command]):Set[Agent] =
    cs.map(_.receiver).toSet

  private def lhsOf(a:Agent,cs:List[Command]):Set[Agent] =
    cs.filter(_.senders contains a).map(_.receiver).toSet

  private def rhsOf(a:Agent,cs:List[Command]):Set[Agent] =
    cs.filter(_.receiver == a).flatMap(_.senders).toSet

  private def gets(guards:List[Guard]):Set[Agent] =
    guards.collect{case g:Get => g.agent}.toSet

  private def unds(guards:List[Guard]):Set[Agent] =
    guards.collect({case g:Und => g.agent}).toSet

  private def overridden(gc: GuardedCommand):Set[Memory] = {
    val undefined = unds(gc.guards).collect({case m:Memory=>m})
    val memories = lhs(gc.commands).collect({case m:Memory=>m})
    memories -- undefined
  }

  private def eventOf(a:Agent,labels:Labels):Event =
    labels.find(l=>l._2.active == a).get._1

  private def label(a:Agent,cs:List[Command],role:Role):Label = role match {
    case Label.Out => Label(a,lhsOf(a,cs),role)
    case _ =>  Label(a,rhsOf(a,cs),role)
  }

  private def inputLabelsOf(gc:GuardedCommand):Labels =
    (lhs(gc.commands)--overridden(gc)).map(a=>seed()->label(a,gc.commands,In)).toMap

  private def outputLabelsOf(gc:GuardedCommand):Labels =
    gets(gc.guards).map(a=>seed()->label(a,gc.commands,Out)).toMap

  private def overriddenLabelsOf(gc:GuardedCommand):Labels =
    overridden(gc).map(a=>seed()->label(a,gc.commands,OverrideIn)).toMap

  private def labels(gc:GuardedCommand):Labels =
    inputLabelsOf(gc)++outputLabelsOf(gc)++overriddenLabelsOf(gc)

  private def order(gc:GuardedCommand,labels:Labels):Set[Order] =
    for (b<-lhs(gc.commands); a<-rhsOf(b,gc.commands))
      yield Order(eventOf(a,labels),eventOf(b,labels))
}


