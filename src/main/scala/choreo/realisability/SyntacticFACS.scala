package choreo.realisability

import choreo.common.MRel.MR
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, Events, NChoice, Nesting}
import choreo.syntax.Choreo.Send
import choreo.syntax.{Agent, Choreo, Msg}

object SyntacticFACS:
  type Error = String

  /** A branching pomset R is well-branched iff, for every inner choice C ≺ R.B,
   * there exist participants a, b such that:
   * - all options are between a and b with different labels
   * - all agents other than a and b cannot distinguish the choice
   * */
  def wellBranched(p:NPomset): Option[Error] =
    wellBranched(p.events,p)

  private def wellBranched(e:Nesting[Event],p:NPomset): Option[Error] =
    for c <- e.choices do
      // check if left is well branched
//      println(s"going recursive on the left ${c.left.show} with choices ${c.left.choices}")
      val wl = wellBranched(c.left,p)
      if wl.nonEmpty then return wl
      // check if right is well branched
//      println(s"going recursive on the right ${c.right.show} with choices ${c.right.choices}")
      val wr = wellBranched(c.right,p)
      if wr.nonEmpty then return wr
      //get leading events
//      println(s"getting leaders (actions: ${p.actions.mkString(",")})")
      val (leadLeft,leadRight) = (leaders(c.left,p.realPred), leaders(c.right,p.realPred))
//      println(s"got ${leadLeft.mkString(",")} and ${leadRight.mkString(",")}")
      // check if there is a single sender
//      println(s"ssender p ${leadLeft.flatMap(getSenders(p)).toList} - ${leadRight.flatMap(getSenders(p)).toList}")
      val (lsnd1,lsnd2) = (leadLeft.flatMap(getSenders(p)) , leadRight.flatMap(getSenders(p)) )
      (lsnd1.toList, lsnd2.toList) match
        case (List(a),List(b)) if a!=b => return Some(s"Different leading senders found: $a in ${c.left.show} and $b in ${c.right.show}")
        case (a::b::_, _) => return Some(s"Different leading senders found in ${c.left.show}: $a and $b")
        case (_, a::b::_) => return Some(s"Different leading senders found in ${c.right.show}: $a and $b")
        case _ =>
      // check if there is a single receiver
//      println(s"srecv p ${leadLeft.flatMap(getReceivers(p)).toList} - ${leadRight.flatMap(getReceivers(p)).toList}")
      val (lrcv1, lrcv2) = (leadLeft.flatMap(getReceivers(p)) , leadRight.flatMap(getReceivers(p)) )
      (lrcv1.toList,lrcv2.toList) match
        case (List(a),List(b)) if a!=b => return Some(s"Different leading receivers found: $a in ${c.left.show} and $b in ${c.right.show}")
        case (a::b::_, _) => return Some(s"Different leading receivers found in ${c.left.show}: $a and $b")
        case (_, a::b::_) => return Some(s"Different leading receivers found in ${c.right.show}: $a and $b")
        case _ =>
      // check if there are no equal messages in these events
//      println(s"eq msg")
      val sharedM = leadLeft.flatMap(getMsg(p)).intersect(leadRight.flatMap(getMsg(p)))
      if sharedM.nonEmpty then
        return Some(s"Shared leading events with the same message when choosing between ${
          c.left.show} and ${c.right.show}: ${sharedM.map(_.names.mkString("/")).mkString(",")}")
      // missing: remaining agents behave the same
      val otherAgents =  e.toSet // set of all events
        .flatMap(p.actions.get) // set of actions (choreo) of these events
        .flatMap(getSends) // set of "as->bs:x" actions, from "ab!x" or "a->b:x" labels
        .flatMap((s:Send) => (s.as++s.bs).toSet ) // set of all agents
        .--(lsnd1++lsnd2++lrcv1++lrcv2) // set agents not involved in the choice
//      println(s"Choice: ${c.left.show} + ${c.right.show}\n Leading: ${
//        (lsnd1++lsnd2++lrcv1++lrcv2).mkString(";")}\n  Others: $otherAgents")
      for ag <- otherAgents do
//        println(s" - 1.$ag: ${NPomset(c.left,p.actions,p.pred,p.loop).project(ag)}")
//        println(s" ----> ${c.left.toSet.filter(e => p.actions.get(e).exists(c => Choreo.agents(c)(ag)))}")
//        println(s" - 2.$ag: ${NPomset(c.right,p.actions,p.pred,p.loop).project(ag)}")
//        println(s" ----> ${c.right.toSet.filter(e => p.actions.get(e).exists(c => Choreo.agents(c)(ag)))}")
        val isom = choreo.npomsets.NPomIso.areIsomorphic(
          NPomset(c.left, p.actions, p.pred, p.loop).project(ag),
          NPomset(c.right, p.actions, p.pred, p.loop).project(ag)
        )
//        println(s" - isomorphorfic? ${isom}")
        if isom.isEmpty then return Some(s"Non-leading agent '$ag' differs after the choice between '${
          NPomset(c.left,  p.actions, p.pred, p.loop).project(ag).events.show}' and '${
          NPomset(c.right, p.actions, p.pred, p.loop).project(ag).events.show}'.")
    for lp <- e.loops do
      val wl = wellBranched(lp, p)
      if wl.nonEmpty then return wl
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


   /** Pairs of sends and pairs of receives on the same channel are properly ordered.
   * I,e,:
   *  - (i) Forall e1:ab!x, e2:ac!y (resp. ?): either
   *    - e1->e2,
   *    - e2->e1, or
   *    - e1#e2 (exclusive)
   *  - (ii) Forall e1:ab!x, e2:ab!y, e3:ab?x, e4:ab?y: either
   *    - e1 is NOT a direct pred. of e3,
   *    - e2 is NOT a direct pred. of e4, or <--- FIX definition!
   *    - if (e1 < e2) then (e3 < e4)
   */
  def wellChanneled(p:NPomset): Option[Error] =
//    val p2 = p.minimized
//    return Some(
//      (for a<-p2.events.toSet yield
//        s"$a: ${p2.realPred(a).mkString(",")}").mkString("\n - ")
//    )
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

//    //// OLD (incorrect)
//    // 3a. for every send e1, check if there is a direct successor e2 - if so, drop every e1 and e2.
//    val simplified = for chn <- channels yield
//      chn._1 -> dropDirectSucc(chn._2,p)
//    // 3b. for all remaining,
//    //     for every pair of senders (e1,e2) with e1<e2, and
//    //     for every match of receivers (e3,e4),
//    //     MUST BE e3 < e4
//    for
//      chn <- simplified.values
//      (e1,e2) <- orderedPairs(chn._1,p)
//      e3 <- findMatches(e1,chn._2)
//      e4 <- findMatches(e2,chn._2)
//    do
//      if !p.isPred(e3._1,e4._1) then return Some(s"${e1._1}${e1._2.pp}<=${
//        e2._1}${e2._2.pp}, but not $${e3._1}${e3._2.pp}<=$${e4._1}${e4._2.pp}.")
//    None

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

//  /** Given a set of sends [(e1,x),...] and receives [(e2,y),...],
//   * drops all pairs of (e1,x) and (e2,y)
//   * when e1 is a direct predecessor of e2 (in p). */
//  private def dropDirectSucc(sndRcv: (Set[(Event,Msg)],Set[(Event, Msg)]), p: NPomset)
//              : (Set[(Event,Msg)],Set[(Event, Msg)]) =
//    if sndRcv._1.isEmpty then sndRcv else
//      val (e1,x1) = sndRcv._1.head
//      val r2 = for (e2,x2) <- sndRcv._2 if x2==x1 && p.realPred(e2)(e1) yield (e2,x2)
////      val (nSnd,nRcv) =
//      if r2.size != sndRcv._2.size
//      then dropDirectSucc((sndRcv._1-(e1->x1), r2), p)
//      else
//        val (s3,r3) = dropDirectSucc((sndRcv._1-(e1->x1), sndRcv._2), p)
//        (s3+(e1->x1),r3)
//
//  /** Given a set of sends [(e1,x),(e2,y)...],
//   * finds all pairs (ei,ej) that are ordered, i.e., that ei<=ej. */
//  private def orderedPairs(evs: Set[(Event, Msg)], p: NPomset): Set[((Event,Msg),(Event,Msg))] =
//    for (e1,x)<-evs; (e2,y)<-evs if e1!=e2 && p.isPred(e1,e2) yield (e1->x, e2->y)
//
//  /** Given a sends (e1,x) and a set of receives [(e2,y),...],
//   * find all (e2,y) such that x==y   */
//  private def findMatches(ev:(Event,Msg), evs: Set[(Event, Msg)]): Set[(Event,Msg)] =
//    for (e2,y)<-evs if y==ev._2 yield (e2,y)


  /** Arrows cannot leave choice boxes, which facilitates proofs. */
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
      val sharedSucc = (leftEv .flatMap(p.allSuccesors(_))--leftEv)
             .intersect(rightEv.flatMap(p.allSuccesors(_))--rightEv)
      if sharedSucc.nonEmpty then
        return Some(s"Found shared successor(s) ${sharedSucc.mkString(",")} to choice [${c.left.show}] OR [${c.right.show}]")

    for lp <- e.loops do
      val wl = treeLike(lp, p)
      if wl.nonEmpty then return wl
    None

  /** All sends and receives are properly paired, dependencies are plausible.
   * I.e., for every e:
   *  1. If there exists e′ ∈ p such that e′ < e
   *     then there exists some event e′′ (not necessarily distinct from e′)
   *     such that e′ ≤ e′′ < e and either
   *       [subj (λ(e′′)) = subj (λ(e))] or
   *       [λ(e′′ ) = ab!x and λ(e) = ab?x for some a, b, x].
   *  2. If λ(e)ab?x and e ∈ p'.topLevel for some p' that is a choice in p
   *     then there exists some e′
   *     such that e′ ∈ p'.topLevel and λ(e′)=ab!x and e′ < e.
   *  3. If λ(e) = ab!x
   *     then there exists exactly one e′
   *     such that e ≤ e′ and
   *              λ(e′)=ab?x
   *              (λ(e'')=ab!x∧e'' ≤e′) ⇒ e'' ≤ e.
   * */
  def choreographic(p: NPomset): Option[Error] =
    var res: List[Error] = Nil
//    var okRcvs = Map[Event,Event]()
//    var otherRcvs = Map[Event,Event]()
    val pMinimized = p.minimized
    for e <- p.events.toSet do
      res :::= choreographic(e)(using pMinimized).toList
//    val missing = otherRcvs -- okRcvs.keySet
//    if missing.nonEmpty then res ::= s"Extra receivers found for an already matched sender ${missing.map(kv=>s"${kv._2}->${kv._1}").mkString(",")}"
    if res.isEmpty then None else Some(res.mkString("\n"))


  /** Check if the event is choreographic by checking 3 cases */
  private def choreographic(e:Event)(using p:NPomset)
      : Option[Error] = //,Map[Event,Event],Map[Event,Event]) =
    val succ = p.succ
//    val (sndErr,okRcvs,otherRcvs) = chorSnd(e,succ)
    val errs = (chorPred(e).toList++chorRcv(e).toList++chorSnd(e,succ).toList) match
      case Nil => None
      case l => Some(l.mkString("\n"))
    errs //,okRcvs,otherRcvs)

  /** Case 1: e:ab?!x
   * every trace of its predecessors must find an event with the same subject or a matching send. */
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
   * must have a matching predecessor at the same level. */
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
   * must have EXACTLY one matching successor with middle e':ab!x
   * (e.g., ab! ab? ab? BAD;   ab! ab! ab? BAD;   ab! ab? ab! .. GOOD */
  private def chorSnd(e:Event, succ:NPomset.Order)(using p:NPomset)
        : Option[Error] = p.actions.get(e) match
    case Some(out@Choreo.Out(a,b,msg)) =>
      val in = Choreo.In(b,a,msg)
      import choreo.common.MRel._
      def findAllMatches(next:Set[Event],senders:Set[Event])
            : MR[Event,Set[Event]] = next.headOption match
//        case Some(e2) if done(e2) => // done
//          findAllMatches(next-e2,senders,done)
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

//
//
//
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


/// OLDER approaches from here


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



