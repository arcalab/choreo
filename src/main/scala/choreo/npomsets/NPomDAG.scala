package choreo.npomsets

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset._
import choreo.datastructures.DAG
import choreo.datastructures.DAG._

/**
 * Created by guillecledou on 12/07/2021
 */

object NPomDAG:
  def apply(p:NPomset):DAG[Event] = DAG.fromPred(p.events.toSet,p.pred)
