package choreo.syntax

import choreo.syntax.{Choreo, Msg}
import choreo.syntax.Choreo._
//import choreo.syntax.Msg._

case class Agent(s: String):
  def !(to: Agent): Out = Out(this, to, Msg(Nil))
  def ? (from: Agent): In = In(this, from, Msg(Nil))
  
  def ->(to: Agent): Send = Send(List(this), List(to), Msg(Nil))
  def →(to: Agent): Send = ->(to)
  def -->(b: Agent): Choreo = (this -> b) > ((b -> this) by Msg.ack)

  override def toString: String = s
