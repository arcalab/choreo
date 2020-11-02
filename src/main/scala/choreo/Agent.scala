package choreo

import scala.util.parsing.input.Positional

/**
 * Created by guillecledou on 28/10/2020
 */


 sealed trait Agent {
   val name:String
}


object Agent {
  final case class Participant(name:String) extends Agent
  final case class Memory(name:String) extends Agent
}
