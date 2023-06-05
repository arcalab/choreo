package choreo.sos

import caos.sos
import caos.sos.SOS
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Msg}


/** Alternative formulation of a Causal Network that uses the `caos.sos.Network` library.
 * Previous formulation can be found in `choreo.sos.Network`, using functions `sosCS` and `mkNetCS`.
 * */
object NetCausal:

  /** Notion of state of the network, and how to print it. */
  case class NSt(queue: Map[(Agent, Agent), List[Msg]]): // type of the network state: a queue
    override def toString: String =
      queue.map(kv => s"${kv._1._1}->${kv._1._2}:[${kv._2.map(_.names).mkString(",")}]").mkString(", ")

  /** Initial state of a network of local specifications. */
  def mkInit[LSt](parts: List[LSt]) = caos.sos.Network.State(parts, NSt(Map()))

  /** SOS semantics of the network, based on `sync` (below), `relabel` (below), and a given local SOS. */
  def sos[LState](localSOS: SOS[Act, LState]) = //: SOS[Act, caos.sos.Network.State[Act,NSt]] =
    caos.sos.Network.sos[Act, Act, LState, NSt](sync, relabel, localSOS)

  /** Core of the semantics: which combinations of actions are valid. */
  def sync(parts: List[Set[Act]], st: NSt): Set[(List[Option[Act]], NSt)] =
    // asynchronous semantics, so any action can evolve alone
    // - What actions can are allowed
    def allow(act: Act) = act match
      case In(a, b, m) => st.queue.contains((b, a)) && st.queue((b, a)).headOption.contains(m)
      case Out(_, _, _) | Internal(_, _) | Tau => true
      case _ => sys.error(s"Unsupported action $act when evolving a projection")

    // - How to make a step as a list of `None` with the `Some(a)` only by participant `i`
    def mkStep(i: Int, a: Act) =
      for (_, n) <- parts.zipWithIndex yield
        if i == n then Some(a) else None

    // - How to update the state of the
    def updSt(act: Act) = act match
      case In(a, b, _) => st.queue + ((b, a) -> st.queue((b, a)).tail) // remove from queue
      case Out(a, b, m) => st.queue + ((a, b) -> (st.queue.getOrElse((a, b), Nil) ::: List(m))) // add to the end of the queue
      case _ => st.queue

    var res = Set[(List[Option[Act]], NSt)]()
    for (set, i) <- parts.zipWithIndex; act <- set if allow(act) do
      res += (mkStep(i, act) -> NSt(updSt(act)))
    res

  /** Converting a valid combination of local actions into a global name. */
  def relabel[LAct](lst: List[Option[LAct]]): LAct =
    lst.map(_.toSet).toSet.flatten.headOption match
      case Some(a) => a
      case None => sys.error(s"No action found when relabelling ${lst.mkString(",")}")

