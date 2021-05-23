package choreo.pomsets

import caos.sos.SOS
import choreo.pomsets.NPomset
import choreo.syntax.Choreo.Action

object NPomDefSOS extends SOS[Action,NPomset]:

  override def accepting(p: NPomset): Boolean = p.accepting

  override def next(p: NPomset): Set[(Action, NPomset)] =
    for
      e <- p.events.toSet // set of events
      p2 <- p.readyFor(e) // some ready pomset
    yield
      p.actions(e) -> (p2-e)


