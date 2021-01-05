package choreo

import choreo.Agent._

import scala.util.parsing.input.Positional

/**
 * Created by guillecledou on 28/10/2020
 */


sealed trait Choreography

object Choreography:

  final case class Interaction(senders:List[Participant],
                               receivers:List[Participant],
                               memories:List[Memory],
                               channelName:String)            extends Choreography
  final case class Seq(c1: Choreography,c2: Choreography)     extends Choreography
  final case class Choice(c1: Choreography,c2: Choreography)  extends Choreography
  final case class Par(c1: Choreography,c2: Choreography)     extends Choreography
  final case class Loop(c: Choreography)                      extends Choreography

