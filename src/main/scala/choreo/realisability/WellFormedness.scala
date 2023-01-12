package choreo.realisability

import choreo.common.MRel.MR
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, Events, NChoice, Nesting}
import choreo.syntax.Choreo.Send
import choreo.syntax.{Agent, Choreo, Msg}

object WellFormedness:
  type Error = String

  /**
   * Checks well-formedness of a branching pomset by checking 4 conditions:
   * (1) well-branchedness,
   * (2) well-channelledness,
   * (3) tree-like shape, and
   * (4) choreographic shape.
   * */
  def checkAll(p:NPomset): List[Error] =
    val wb = wellBranched(p)
    val wc = wellChanneled(p)
    val tl = treeLike(p)
    val ch = choreographic(p)
    if wc.isEmpty && wb.isEmpty && tl.isEmpty && ch.isEmpty then Nil
    else List(
      if wb.isEmpty then "Well-branched" else s"NOT well-branched: ${wb.get}",
      if wc.isEmpty then "Well-channeled" else s"NOT well-channeled: ${wc.get}",
      if tl.isEmpty then "Tree-like" else s"NOT tree-like: ${tl.get}",
      if ch.isEmpty then "Choreographic" else s"NOT choreographic: ${ch.get}"
    )

  ///////////////////////

  /** A branching pomset `R' is well-branched iff, for every inner choice `C ≺ R.B',
   * there exist participants `a', `b' such that:
   * (1) there is a single initial event in each option,
   * (2) these initial events are between `a' and `b' differing only on the labels, and
   * (3) all agents other than `a' and `b' cannot distinguish the choice.
   * */
  def wellBranched(p:NPomset): Option[Error] =
    wellBranched(p.events,p)

  private def wellBranched(e:Nesting[Event],p:NPomset): Option[Error] =
    for c <- e.choices do
      // - Check if left is well branched
      // println(s"going recursive on the left ${c.left.show} with choices ${c.left.choices}")
      val wl = wellBranched(c.left,p)
      if wl.nonEmpty then return wl
      // check if right is well branched
//      println(s"going recursive on the right ${c.right.show} with choices ${c.right.choices}")
      val wr = wellBranched(c.right,p)
      if wr.nonEmpty then return wr

      // - Get leading events
      // println(s"getting leaders (actions: ${p.actions.mkString(",")})")
      val (leadLeft,leadRight) = (leaders(c.left,p.realPred), leaders(c.right,p.realPred))
      //println(s"got ${leadLeft.mkString(",")} and ${leadRight.mkString(",")}")
      // - Check if there is a single sender
      //println(s"ssender p ${leadLeft.flatMap(getSenders(p)).toList} - ${leadRight.flatMap(getSenders(p)).toList}")
      val (lsnd1,lsnd2) = (leadLeft.flatMap(getSenders(p)) , leadRight.flatMap(getSenders(p)) )
      (lsnd1.toList, lsnd2.toList) match
        case (List(a),List(b)) if a!=b => return Some(s"Different leading senders found: $a in ${c.left.show} and $b in ${c.right.show}")
        case (a::b::_, _) => return Some(s"Different leading senders found in ${c.left.show}: $a and $b")
        case (_, a::b::_) => return Some(s"Different leading senders found in ${c.right.show}: $a and $b")
        case _ =>
      // - Check if there is a single receiver
      // println(s"srecv p ${leadLeft.flatMap(getReceivers(p)).toList} - ${leadRight.flatMap(getReceivers(p)).toList}")
      val (lrcv1, lrcv2) = (leadLeft.flatMap(getReceivers(p)) , leadRight.flatMap(getReceivers(p)) )
      (lrcv1.toList,lrcv2.toList) match
        case (List(a),List(b)) if a!=b => return Some(s"Different leading receivers found: $a in ${c.left.show} and $b in ${c.right.show}")
        case (a::b::_, _) => return Some(s"Different leading receivers found in ${c.left.show}: $a and $b")
        case (_, a::b::_) => return Some(s"Different leading receivers found in ${c.right.show}: $a and $b")
        case _ =>
      // - Check if there are no equal messages in these events
      // println(s"eq msg")
      val sharedM = leadLeft.flatMap(getMsg(p)).intersect(leadRight.flatMap(getMsg(p)))
      if sharedM.nonEmpty then
        return Some(s"Shared leading events with the same message when choosing between ${
          c.left.show} and ${c.right.show}: ${sharedM.map(_.names.mkString("/")).mkString(",")}")
      // - Check if remaining agents behave the same
      val otherAgents =  e.toSet // set of all events
        .flatMap(p.actions.get) // set of actions (choreo) of these events
        .flatMap(getSends) // set of "as->bs:x" actions, from "ab!x" or "a->b:x" labels
        .flatMap((s:Send) => (s.as++s.bs).toSet ) // set of all agents
        .--(lsnd1++lsnd2++lrcv1++lrcv2) // set agents not involved in the choice
      // println(s"Choice: ${c.left.show} + ${c.right.show}\n Leading: ${
      //   (lsnd1++lsnd2++lrcv1++lrcv2).mkString(";")}\n  Others: $otherAgents")
      for ag <- otherAgents do
        // println(s" - 1.$ag: ${NPomset(c.left,p.actions,p.pred,p.loop).project(ag)}")
        // println(s" ----> ${c.left.toSet.filter(e => p.actions.get(e).exists(c => Choreo.agents(c)(ag)))}")
        // println(s" - 2.$ag: ${NPomset(c.right,p.actions,p.pred,p.loop).project(ag)}")
        // println(s" ----> ${c.right.toSet.filter(e => p.actions.get(e).exists(c => Choreo.agents(c)(ag)))}")
        val isom = choreo.npomsets.NPomIso.areIsomorphic(
          NPomset(c.left, p.actions, p.pred, p.loop).project(ag),
          NPomset(c.right, p.actions, p.pred, p.loop).project(ag)
        )
        // println(s" - isomorphorfic? ${isom}")
        if isom.isEmpty then return Some(s"Non-leading agent '$ag' differs after the choice between '${
          NPomset(c.left,  p.actions, p.pred, p.loop).project(ag).events.show}' and '${
          NPomset(c.right, p.actions, p.pred, p.loop).project(ag).events.show}'.")
    // - Check if loops are well-branched
    for lp <- e.loops do
      val wl = wellBranched(lp, p)
      if wl.nonEmpty then return wl
    // - No errors were found if it reached this point
    None

  //////
  // Auxiliar functions for well-branched
  //////
  private def leaders(n:Nesting[Event], pred:Event => Set[Event]): Set[Event] =
    val s = n.toSet
    s.filter( e => s.intersect(pred(e)).isEmpty)
  private def getSends(p:NPomset)(e:Event): Set[Send] =
    p.actions.get(e).toSet.flatMap(getSends)
  private def getSends(c:Choreo): Set[Send] = c match
    case s:Send => Set(s)
    case Choreo.Seq(c1,c2)     => getSends(c1)++getSends(c2)
    case Choreo.Par(c1,c2)     => getSends(c1)++getSends(c2)
    case Choreo.Choice(c1,c2)  => getSends(c1)++getSends(c2)
    case Choreo.DChoice(c1,c2) => getSends(c1)++getSends(c2)
    case Choreo.Out(a,b,m)     => Set(Send(List(a),List(b),m))
    case Choreo.In(b,a,m)      => Set(Send(List(a),List(b),m))
    case _ => Set()
  private def getSenders(p:NPomset)(e:Event): Set[Agent] =
    getSends(p)(e).flatMap(_.as)
  private def getReceivers(p:NPomset)(e:Event): Set[Agent] =
    getSends(p)(e).flatMap(_.bs)
  private def getMsg(p:NPomset)(e:Event): Set[Msg] =
    getSends(p)(e).map(_.m)

  ///////////////////////

   /** A branching pomset is well-channeled if pairs of sends and pairs of receives
    * on the same channel are properly ordered.
   * I,e,:
   *  - (i) Forall e1:ab!x, e2:ac!y (resp. ?): either
   *    - e1->e2,
   *    - e2->e1, or
   *    - e1#e2 (exclusive); and
   *  - (ii) Forall e1:ab!x, e2:ab!y, e3:ab?x, e4:ab?y: either
   *    - e1 is NOT a direct pred. of e3,
   *    - e2 is NOT a direct pred. of e4, or
   *    - if (e1 < e2) then (e3 < e4)
   */
  def wellChanneled(p:NPomset): Option[Error] =
    ///// (i)
    // 1. collect channels, e.g.: ab --> {(e1,x),(e2,y)}, {(e3,x),(e4,y)}
    val channels = getChannels(p.actions)
    // 2. for every combination, e.g. (e1,e2), check if related or exclusive
    for
      chn <- channels.values
      (snd1,x) <- chn._1
      (snd2,y) <- chn._1 if (snd1,x)!=(snd2,y)
    do
      if !relatedOrExclusive(snd1,snd2,p)
      then return Some(s"Sending events $snd1 and $snd2 are neither related nor disjoint.")
    // 2b. do the same for receivers
    for
      chn <- channels.values
      (rcv1, x) <- chn._2
      (rcv2, y) <- chn._2 if (rcv1, x) != (rcv2, y)
    do
      if !relatedOrExclusive(rcv1, rcv2, p)
      then return Some(s"Receiving events $rcv1 and $rcv2 are neither related nor disjoint.")
    ///// (ii)
    // corrected version:
    val pm = p.minimized
    // for every channel ab
    for (ags,evs) <- channels do
      // 3a) collect every receiver e3, and its direct successor e1 in senders -> (e1,e3)
      val matches = for
        (e3,x) <- evs._2
        e1 <- pm.realPred(e3) if
          pm.actions(e1) match {case _: Choreo.Out => true; case _=> false}
      yield e1->e3
      // 3b) for pair of pairs (e1,e3), (e2,e4), if e1<e2 then it must be e3<e4
      for
        (e1,e3) <- matches
        (e2,e4) <- matches if e1!=e2 && e3!=e4
      do
        //println(s"Checking if $e1<$e2 implies $e3<$e4")
        //// for each channel with N interaction, performing N*(N-1) comparisons
        if pm.isPred(e1,e2) && !pm.isPred(e3,e4)
        then return Some(s"Event $e1<$e2 but not $e3<$e4.")
    None

  //////
  // Auxiliar functions for well-channeled
  //////
  private def getChannels(acts:Actions): Map[(Agent,Agent),(Set[(Event,Msg)],Set[(Event,Msg)])] =
    var res = Map[(Agent,Agent),(Set[(Event,Msg)],Set[(Event,Msg)])]()
    def addAct(elem:(Event,Choreo)):Unit = elem._2 match
      case Choreo.Par(c1, c2) => addAct(elem._1->c1); addAct(elem._1->c2)
      case Choreo.In(a, b, m) => res.get((b,a)) match
          case Some((snds,rcvs)) => res += (b,a) -> (snds,rcvs+(elem._1->m))
          case None              => res += (b,a) -> (Set(),Set(elem._1->m))
      case Choreo.Out(a, b, m) => res.get((a,b)) match
          case Some((snds,rcvs)) => res += (a,b) -> (snds+(elem._1->m),rcvs)
          case None              => res += (a,b) -> (Set(elem._1->m),Set())
      case Choreo.Tau =>
      case Choreo.Internal(_, _) =>
      case _ => sys.error(s"Well-channeledness of b-pomsets only defined for atomic labels (or sets) - failed at ${elem._1}:${elem._2}")
    acts.foreach(addAct)
    res

  /** Checks if e1<=e2 or e2<=e1 or e1+e2 are disjoint */
  private def relatedOrExclusive(e1: Event, e2: Event, p: NPomset): Boolean =
    p.isPred(e1,e2) || p.isPred(e2,e1) || p.isExclusive(e1,e2)

  ///////////////////////

  /** A branching pomset is tree-like if causality arrows
   * cannot leave choice boxes (which facilitates proofs). */
  def treeLike(p: NPomset): Option[Error] =
    treeLike(p.events,p)

  private def treeLike(e:Nesting[Event], p: NPomset): Option[Error] =
    for c <- e.choices do
      // check if left-side is tree-like
      val tl = treeLike(c.left, p)
      if tl.nonEmpty then return tl
      // check if right-side is tree-like
      val tr = treeLike(c.right, p)
      if tr.nonEmpty then return tr
      // check if shared successors are disjoint
      val (leftEv,rightEv) = (c.left.toSet, c.right.toSet)
      val leftSucc =  leftEv.flatMap(p.allSuccesors(_))--leftEv
      val rightSucc = rightEv.flatMap(p.allSuccesors(_))--rightEv
      if leftSucc.nonEmpty then
        return Some(s"Found successor(s) [${leftSucc.mkString(",")}] of choice [${c.left.show}]")
      if rightSucc.nonEmpty then
        return Some(s"Found successor(s) [${rightSucc.mkString(",")}] of choice [${c.right.show}]")

    // check if loops are also tree-like
    for lp <- e.loops do
      val wl = treeLike(lp, p)
      if wl.nonEmpty then return wl
      val loopEv = lp.toSet
      val loopSucc =  loopEv.flatMap(p.allSuccesors(_))--loopEv
      if loopSucc.nonEmpty then
        return Some(s"Found successor(s) [${loopSucc.mkString(",")}] of choice [${lp.show}]")

    // no errors found - return None
    None

  ///////////////////////

  /** A branching pomset is choreographic if all sends and receives
   * are properly paired, and dependencies are plausible.
   * I.e., for every e:
   *  1. For every e′ < e
   *     there exists some event e′′ (not necessarily distinct from e′)
   *     such that e′ ≤ e′′ < e and either
   *       (i)  [subj(λ(e′′)) = subj(λ(e))] or
   *       (ii) [λ(e′′)=ab!x and λ(e)=ab?x for some a, b, x].
   *  2. If λ(e)=ab?x and e is in a choice
   *     then there exists some e′ in the same choice branch (not further nested)
   *     such that λ(e′)=ab!x and e′ < e.
   *  3. If λ(e)=ab!x
   *     then there exists exactly one e′
   *     such that e ≤ e′ and
   *              λ(e′)=ab?x
   *              forall e′′: (λ(e)=ab!x ∧ e′′≤e′) ⇒ e≤e.
   * */
  def choreographic(p: NPomset): Option[Error] =
    var res: List[Error] = Nil
    val pMinimized = p.minimized
    for e <- p.events.toSet do
      res :::= choreographic(e)(using pMinimized).toList
    if res.isEmpty then None else Some(res.mkString("\n"))


  /** Check if the event is choreographic by checking 3 cases */
  private def choreographic(e:Event)(using p:NPomset)
      : Option[Error] =
    val succ = p.succ
    (chorPred(e).toList++chorRcv(e).toList++chorSnd(e,succ).toList) match
      case Nil => None
      case l => Some(l.mkString("\n"))

  /** Case 1: e:ab?!x
   * - every trace of its predecessors must find an event with the
   *   same subject or a matching send. */
  private def chorPred(e:Event)(using p:NPomset): Option[Error] = p.actions.get(e) match
    case Some(act:(Choreo.In|Choreo.Out)) =>
      def subj(c:Choreo) = c match
        case a:Choreo.Action => a.subj
        case _ => sys.error("only supports choreographic analysis of labels as single actions")
      if p.realPred(e).isEmpty || p.pred.getOrElse(e,Set()).forall(e2=>goodPred(e2,ch => {
        //println(s"---$e/$e2:\n- act.isInstanceOf[Choreo.In]: ${act.isInstanceOf[Choreo.In]}\n- ch == act.dual: ${ch == act.dual}\n- subj(ch) == subj(act): ${subj(ch) == subj(act)}")
        (act.isInstanceOf[Choreo.In] && ch == act.dual) || subj(ch) == subj(act)
      }))
      then None
      else Some(s"Found a predecessor of ${e} with no intermediate matching events or with the same subject.")
    case _ => None

  private def goodPred(e:Event, chk:Choreo=>Boolean)(using p:NPomset): Boolean =
    chk(p.actions(e)) ||
      (p.pred.getOrElse(e,Set()).nonEmpty &&
       p.pred.getOrElse(e,Set()).forall(goodPred(_,chk)))

  /** case 2: e:ab?x in a choice
   *  - it must have a matching predecessor at the same level. */
  private def chorRcv(e:Event)(using p:NPomset): Option[Error] = p.actions.get(e) match
    case Some(Choreo.In(a,b,msg)) if !p.events.acts(e) => // a non-top receive
      val es = findNeighbours(e,p.events).intersect(p.allRealPred(e))
      if es.exists(e2 => p.actions.get(e2).contains(Choreo.Out(b,a,msg)))
      then None
      else Some(s"No maching send of $e found in the same choice (neighbour predecessors: [$es])")
    case _ => None

  private def findNeighbours(e:Event,evs:Events): Set[Event] =
    if evs.acts contains e
    then evs.acts-e
    else
      evs.choices.flatMap(c => findNeighbours(e,c.left) ++ findNeighbours(e,c.right)) ++
      evs.loops.flatMap(lp => findNeighbours(e,lp))

  /** Case 3: e:ab!x
   * - it must have EXACTLY one matching successor that has no intermediate e′:ab!x
   * (e.g. BAD:  ab!-ab?-ab?  ;   ab!-ab!-ab? BAD )
   * (e.g. GOOD: ab!-ab?-ab!.. ;  ab!<(ab!||ab?)>ab? )
   * */
  private def chorSnd(e:Event, succ:NPomset.Order)(using p:NPomset)
        : Option[Error] = p.actions.get(e) match
    case Some(out@Choreo.Out(a,b,msg)) =>
      val in = Choreo.In(b,a,msg)
      import choreo.common.MRel._
      def findAllMatches(next:Set[Event],senders:Set[Event])
            : MR[Event,Set[Event]] = next.headOption match
        case Some(e2) if p.actions.get(e2).contains(out) => // found sender
          val succs = findAllMatches(succ.getOrElse(e2,Set()),senders+e2)
          val others = findAllMatches(next-e2,senders)
          succs :++ others
        case Some(e2) if p.actions.get(e2).contains(in) => // found receiver
          mkMR(e2->senders) :++
          findAllMatches(succ.getOrElse(e2,Set())++(next-e2),senders)
        case Some(e2) =>
          findAllMatches(succ.getOrElse(e2,Set())++(next-e2),senders)
        case None =>
          mkMR()

      val matches = findAllMatches(succ.getOrElse(e,Set()),Set())
      val m2 = for (rcv,snds) <- matches if snds.flatten.isEmpty yield rcv
      m2.size match
        case 0 => Some(s"No receives were found for $e")
        case 1 => None
        case _ => Some(s"Found multiple receives [${m2.mkString(",")}] for sender $e")

    case _ => None


/// OLDER approaches from here

//
//      /** Find the closest receiver matching 'e' */
//      def findRcv(next:Set[Event],done:Set[Event])
//          : (Option[Error],Map[Event,Event],Map[Event,Event]) = next.headOption match
//        case Some(e2) if done(e2) =>
//          findRcv(next-e2,done)
//        case Some(e2) if p.actions.get(e2).contains(out) =>
//          //Some(s"Found consecutive senders without a receiver \"$e\" -> \"${e2}\".")
//          findRcv(next-e2,done+e2) // stop branch
//        case Some(e2) if p.actions.get(e2).contains(in) =>
//          findOtherRcv(succ.getOrElse(e2,Set())++(next-e2),Set(), e2)
//        case Some(e2) =>
//          //println(s"skipping $e2 because it is not $in nor $out")
//          findRcv(succ.getOrElse(e2,Set())++(next-e2),done+e2) // skip event
//        case None =>
//          (Some(s"No matching receiver for $e found."),Map(),Map())
//      /** find other receives that can be reached from 'e' without passing by sends */
//      def findOtherRcv(next:Set[Event],done:Set[Event],orig:Event)
//          : (Option[Error],Map[Event,Event],Map[Event,Event]) = next.headOption match
//        case Some(e2) if done(e2) =>
//          findOtherRcv(next-e2,done, orig)
//        case Some(e2) if p.actions.get(e2).contains(in) =>
//          //Some(s"Found two receivers \"$orig\" and \"$e2\" for $e.")
//          val (err,ok,other) = findOtherRcv(succ.getOrElse(e2,Set())++(next-e2),done-e2,orig)
//          (err,ok,other+(e2->e)) // found other receiver
//        case Some(e2) if p.actions.get(e2).contains(out) =>
//          findOtherRcv(next-e2, done+e2, orig) // stop branch
//        case Some(e2) =>
//          findOtherRcv(succ.getOrElse(e2,Set())++(next-e2), done+e2, orig) // continue
//        case None =>
//          (None,Map(orig->e),Map())
//
//      findRcv(succ.getOrElse(e,Set()),Set())
//    case _ => (None,Map(),Map())



//  private def choreographic(e: Event)(using p: NPomset): Option[Error] = p.actions.get(e) match
//    // Our implementation in 3 steps, addressing (2), (3), then (1), respectively.
//    // i) if e is a rcv, collect its set P of predecessors, and
//    //          check recursively on the branching structure if:
//    //     - the snd exists outside and in P, or
//    //     - for some set of choices where "e" does not occur, the snd always exist (and in P), or
//    //     - if "e" is in some set of choices, check recursively, else fail
//    // i) FIXED attempt
//    //    if e is a rcv, collect its predecessors P and
//    case Some(Choreo.In(a,b,msg)) =>
//      val pred = p.allRealPred(e)
//      findMatchSnd(e, Choreo.Out(b,a,msg),pred,p.events)
//
//    // ii) if e is a snd, collect its set of DIRECT successors P, and check if:
//    //     - exactly one is a matching rcv
//    case Some(Choreo.Out(a,b,msg)) =>
//      val minPred = p.pred // p is minimised once for all events @ choreographic(p:NPomset)
//      val succ = for
//          rcv <- p.events.toSet
//          if minPred.getOrElse(rcv,Set()).contains(e) && p.actions.get(rcv).contains(Choreo.In(b,a,msg))
//      yield rcv
//      if succ.isEmpty then return Some(s"No direct successor of $e found with label ${Choreo.In(b,a,msg)}.")
//      if succ.size > 1 then return Some(s"Multiple direct successors of $e found with label ${Choreo.In(b,a,msg)} (${succ.mkString(", ")}).")
//
//      // iii) if e is a snd and has some predecessor, then
//      //     - traverse predecessors: all paths must contain some e'' with the same subject.
//      if minPred.getOrElse(e,Set()).nonEmpty then
//        return findAlwaysSubject(a,minPred,minPred(e),e)
//      //else println(s"No predecessors of $e")
//
//      return None
//
//    case Some(x) => Some(s"Unsupported label $x by event $e.")
//    case None => Some(s"No label found for event $e.")
//
//  //////
//  // Auxiliar functions for choreographic
//  //////
//  private def findMatchSnd(e:Event, snd:Choreo.Out,pred:Set[Event],evs:NPomset.Events)
//                          (using p:NPomset): Option[Error] =
//    // snd exists outside the event structure and is a predecessor
//    if evs.acts.exists(e => pred(e) && p.actions.get(e).contains(snd)) then
//      return None
//    // exists a choice without the event e and with a guaranteed sender
//    if evs.choices.exists(c => !c.left.toSet(e) && !c.right.toSet(e) && mustHave(e,c,pred)) then
//      return None
//    // e exists outside the event structure (not in a choice nor loop)
//    if evs.acts(e) then return Some(s"Failed to find matching receiver $snd for event $e")
//    // otherwise keep searching in the choice where "e" is
//    for c <- evs.choices if c.left.toSet.contains(e) do
//      return findMatchSnd(e,snd,pred,c.left)
//    for c <- evs.choices if c.right.toSet.contains(e) do
//      return findMatchSnd(e, snd, pred, c.right)
//    for c <- evs.loops if c.toSet.contains(e) do
//      return findMatchSnd(e, snd, pred, c)
//    // if no "e" is found in any choice, then it was in a loop (or it was a bad pomset) - fail
//    return Some(s"Failed to find matching receiver $snd for event $e (event not found)")
//
//
//  private def mustHave(e:Event, c:NChoice[Event], pred:Set[Event]): Boolean =
//    mustHave(e,c.left,pred) && mustHave(e,c.right,pred)
//
//  private def mustHave(e: Event, evs: NPomset.Events, pred: Set[Event]): Boolean =
//    evs.acts(e) || evs.choices.exists(c => mustHave(e,c,pred))
//
//  private def findAlwaysSubject(a:Agent, pred:NPomset.Order, evs:Set[Event], origin:Event)
//                               (using p:NPomset): Option[Error] =
//    //println(s"searching for $a in [${evs.mkString(",")}] from $origin")
//    for ev<-evs if !p.actions.get(ev).match {
//      case Some(act: Choreo.Action) => act.subj.contains(a)
//      case _ => false
//    } do
//      val next = pred.getOrElse(ev,Set())
//      if next.isEmpty then
//        return Some(s"Failed to find an event by $a in path $ev --> $origin.")
//      val cont = findAlwaysSubject(a,pred,next,origin)
//      if cont.nonEmpty then return cont
//    None



