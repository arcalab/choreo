package choreo.oldChoreo

import choreo.oldChoreo.GuardedCommand
import choreo.oldChoreo.Agent._

/**
 * Created by guillecledou on 28/10/2020
 */


case class Channel(name: String,
                   senders: List[Participant],
                   receivers: List[Participant],
                   memories: List[Memory],
                   guardedCommands: List[GuardedCommand]) {
}
