package choreo.projection

import choreo.common.Simplify
import choreo.npomsets.NPomset
import choreo.syntax.Agent
import choreo.syntax.Choreo.agents

/** Default projection of pomsets. */
object NPomDefProj extends Projection[Agent,NPomset]:
  def getElements(p: NPomset): Set[Agent] = p.actions.values.flatMap(agents(_)).toSet
  def proj(p:NPomset, a:Agent): NPomset = p.project(a)

