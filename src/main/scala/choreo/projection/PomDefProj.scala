package choreo.projection

import choreo.common.Simplify
import choreo.pomsets.Pomset
import choreo.syntax.Agent

/** Default projection of pomsets. */
object PomDefProj extends Projection[Agent,Pomset]:
  def getElements(p: Pomset): Set[Agent] = p.agents
  def proj(p:Pomset, a:Agent): Pomset = p.project(a)

