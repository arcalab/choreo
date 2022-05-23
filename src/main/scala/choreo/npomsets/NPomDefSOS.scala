package choreo.npomsets

import caos.sos.SOS
import choreo.syntax.Choreo.Action

object NPomDefSOS extends SOS[Action,NPomset]:

  override def accepting(p: NPomset): Boolean = p.accepting

  override def next(p: NPomset): Set[(Action, NPomset)] =
//    println("Ola")
    for
      e <- p.events.toSet // set of events
      (p2,e_) <- p.readyFor3(e) // some ready pomset with actual event fired e_ (could be fresh)
    yield
      p2.actions(e_) -> (p2-e_)


