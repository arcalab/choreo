package choreo.syntax

import cats.implicits._
import choreo.syntax.GlobalContext._
import choreo.{Agent, Channel}

import scala.util.parsing.input.Positional


/**
 * Created by guillecledou on 29/10/2020
 */


case class GlobalContext(channels:Context[CEntry],agents:Context[AEntry]) {

  def addChannel(name:String,ch:CEntry):CtxErrorOr[GlobalContext] =
    channels.addEntry(name,ch).map(chs=>GlobalContext(chs,agents))

  def addAgent(name:String,e:AEntry):CtxErrorOr[GlobalContext] = {
    if (agents.contains(name) && e != agents.context(name)) Either.right(this)
    else agents.addEntry(name,e).map(ags=>GlobalContext(channels,ags))
  }

  def getChannel(name:String):CtxErrorOr[Channel] = channels.getEntry(name).map(c=> c.channel)

  def getAgent(name:String):CtxErrorOr[Agent] = agents.getEntry(name).map(a => a.agent)

  def containsAgent(name:String):Boolean = agents.contains(name)

  def containsChannel(name:String):Boolean = channels.contains(name)

  def newScope():GlobalContext =
    GlobalContext(channels,Context[AEntry](Map()))

}

object GlobalContext {
  type Ctx[A] = Map[String,A]
  type CtxErrorOr[A] = Either[ContextError,A]

  case class ContextError(msg:String)

  case class Context[A<:Positional](context:Ctx[A]) {

    def addEntry(name:String,entry:A): CtxErrorOr[Context[A]] =
      if (context contains(name)) Either.left(
        ContextError(s"Multiple declaration for name $name, first found at ${context(name).pos}"))
      else Either.right(Context[A](context+(name->entry)))

    def getEntry(name:String):CtxErrorOr[A] =
      if (context.contains(name)) Either.right(context(name))
      else Either.left(ContextError(s"Undefined name $name"))

    def contains(name:String):Boolean = context.contains(name)

  }

  def apply():GlobalContext = GlobalContext(Context[CEntry](Map()),Context[AEntry](Map()))

  case class CEntry(channel: Channel) extends Positional

  sealed trait AEntry extends Positional {
    val agent:Agent
  }

  case class InEntry(agent:Agent) extends AEntry
  case class OutEntry(agent:Agent) extends AEntry
  case class MemEntry(agent:Agent) extends AEntry


}
