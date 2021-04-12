package choreo.choreo2.syntax

import choreo.choreo2.syntax.Msg

case class Msg(l:List[String]):
  def pp:String = if l.isEmpty then "" else ":"+l.mkString("/")
  def names:String = l.mkString("/")
  def +(m:String): Msg = Msg(m::l)
  def ++(m:Msg): Msg = Msg(m.l:::l)

object Msg:
  val ack: Msg = Msg(List("ack"))