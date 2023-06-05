package choreo.sos

import caos.sos.SOS
import caos.sos.Network.State as CaosNetState
import choreo.sos.ChorSyncSOS.Interact
import choreo.syntax.{Agent, Choreo, Msg}

object NetSync:

  // Step 1: what is a state

  /** We fix the indexing set I of agents is given by choreo.syntax.Agent. */
  type I = Agent
  /** We fix actions to be interactions by choreo.sos.ChorSyncSOS.Interact. */
  type GAct = Interact
  type LAct = Interact
  /** We fix messages to be given by choreo.syntax.Msg. */
  type M = Msg

  /** A synchronisation type describes the possible number of senders and receivers.
   * Note: in the literature it is usually used a (possibly unbounded) interval instead of a set. */
  type SyncType = Map[M,(Set[Int],Set[Int])]

  /** Notion of state of the network, and how to print it. */
  case class NetState[GSt](agents: List[I], eqs: I=>(GSt=>Set[GSt]), syncT: SyncType):
    override def toString: String =
      s"Agents: [${agents.mkString(",")}], sync-type: [${
        syncT.map((m,as)=>s"{${as._1.mkString(",")}}->{${as._2.mkString(",")}}:$m")}]"
      //proj.map(kv => s"${kv._1}: ${kv._2}").mkString("\n")

  // Step 2: what is the initial state

  def mkInit[GSt](st:GSt,
                  ags: Set[I],
                  eqs: I=>(GSt=>Set[GSt]),
                  syncT: SyncType): CaosNetState[Set[GSt],NetState[GSt]] =
    val agsLst = ags.toList
    CaosNetState(
      agsLst.map(ag => eqs(ag)(st)),
      NetState( ags.toList, eqs /*ags(st).map(ag=> ag->eqs(ag)(st))*/ , syncT) // fixing the order of agents
    )

  // Step 3: how to synchronise 2 labels

  private def sync[GSt](upd: I=>GAct=>Option[LAct])
                       (parts: List[Set[LAct]], st: NetState[GSt]): Set[(List[Option[LAct]], NetState[GSt])] =
    // each participant has a list of actions. For each combination (powerset), just check the sync-type
    // part 1: collect, for each message, the set of possible senders and receivers
    var msgs = Map[Msg, (Set[Agent], Set[Agent])]() // map messages to input and ouput agents that can evolve by it
    for (acts,ag) <- parts.zip(st.agents); Interact(as, bs, m) <- acts do //.flatMap(upd(ag)) do
      val (ins, outs) = msgs.getOrElse(m, Set() -> Set())
      val ins2 = if as.contains(ag) then ins + ag else ins
      val outs2 = if bs.contains(ag) then outs + ag else outs
      msgs += m -> (ins2, outs2)
    // part 2: for each message, find combinations accepted by the sync-type
    val comm =
      for
        (m, (ins, outs)) <- msgs.toSet // for each message (with possible senders and receivers)
        is <- ps(ins) // for any combination of senders...
        os <- ps(outs) // ... and of receivers
        if // (is.nonEmpty||os.nonEmpty) //&&
          st.syncT.contains(m) && (// if the sync-type allows that message with that combination
            st.syncT(m)._1.contains(is.size) &&
              st.syncT(m)._2.contains(os.size))
      yield
        // then it can evolve "proj" by "is->os:m"
        st.agents.map(ag => upd(ag)(Interact(is,os,m))) //if is(ag)||os(ag) then Some(Interact(is,os,m).filter(ag)) else None)
          -> st // state unchanged (?)
    //println(s"### Checking comm of ${parts.size} participants: $parts.\n### Msg: ${msgs.mkString(",")}\n### Inferred:\n - ${comm.map(_._1)}\n---")
    comm


  /** Power set of a given set. */
  private def ps[A](s: Set[A]): Set[Set[A]] = s.headOption match
    case None => Set(Set())
    case Some(e) => (ps(s - e).map(_ + e) ++ ps(s - e)) + s


  // Step 4: how to convert a vector of possible local actions (interaction) into a single global action (interaction)

  private def relabel(lst: List[Option[LAct]]): GAct =
    // union of all receivers and senders, and assuming the same message
    Interact( lst.toSet.flatMap(_.toSet.flatMap(act=>act.from)), // from
              lst.toSet.flatMap(_.toSet.flatMap(act=>act.to)),   // to
              lst.toSet.flatMap(_.toSet.map(act=>act.m))         // msg
                .headOption.getOrElse(sys.error("Not supported: combination of 0 local steps")) )
    //println(s"$$$$$$ asked to relabel [${lst.mkString(",")}]; got $res.")

  // Step 5: how to evolve a step using the Quotient SOS

  // quotients must reveal only needed info
  private def localSOSs(updAct:I => GAct => Option[LAct])
                       (nst: NetState[Choreo]): List[SOS[GAct, Set[Choreo]]] =
    nst.agents.map(ag => Quotient[GAct, LAct, Choreo](nst.eqs(ag), updAct(ag), ChorSyncSOS))

  // Step 6: build the SOS of the network, combining steps 3 (sync), 4 (relabel), and 5 (local SOS)

  /** SOS semantics of the network, based on `sync`, `relabel`, and `localSOSs` (above). */
  def sos(updAct:I => GAct => Option[LAct]) : SOS[GAct, caos.sos.Network.State[Set[Choreo],NetState[Choreo]]] =
    caos.sos.Network.sos[GAct, LAct, Set[Choreo], NetState[Choreo]](
      sync(updAct), // given a (global) state, for each agent and set of available actions, compute combinations of actions and their destination (global) state
      relabel, // change a combination of actions (from each agent) into a
      localSOSs(updAct)// using here the Quotient SOS with underlying Sync SOS
    )

  /*
  case class Quotient[GAct,LAct,St](eqs: St => Set[St],
                                    updAct: GAct => Option[LAct],
                                    sos: SOS[GAct,St]) extends SOS[LAct,Set[St]]:

  */



