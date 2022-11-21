package choreo.realisability

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, Nesting}
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
   *  - Forall e1:ab!x, e2:ac!y (resp. ?): either
   *    - e1->e2,
   *    - e2->e1, or
   *    - e1#e2 (exclusive)
   *  - Forall e1:ab!x, e2:ab!y, e3:ab?x, e4:ab?y: either
   *    - e1 is a direct pred. of e3,
   *    - e2 is a direct pred. of e4, or
   *    - if (e1 < e2) then (e3 < e4)
   */
  def wellChanneled(p:NPomset): Option[Error] =
    // 1. collect channels, e.g.: ab --> {(e1,x),(e2,y)}, {(e3,x),(e4,y)}
    val channels = getChannels(p.actions)
    // 2. for every combination, e.g. (e1,e4), check if related or exclusive
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
    // 3a. for every send e1, check if there is a direct successor e2 - if so, drop every e1 and e2.
    val simplified = for chn <- channels yield
      chn._1 -> dropDirectSucc(chn._2,p)
    // 3b. for all remaining,
    //     for every pair of senders (e1,e2) with e1<e2, and
    //     for every match of receivers (e3,e4),
    //     MUST BE e3 < e4
    for
      chn <- simplified.values
      (e1,e2) <- orderedPairs(chn._1,p)
      e3 <- findMatches(e1,chn._2)
      e4 <- findMatches(e2,chn._2)
    do
      if !p.isPred(e3._1,e4._1) then return Some(s"${e1._1}${e1._2.pp}<=${
        e2._1}${e2._2.pp}, but not $${e3._1}${e3._2.pp}<=$${e4._1}${e4._2.pp}.")
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

  /** Given a set of sends [(e1,x),...] and receives [(e2,y),...],
   * drops all pairs of (e1,x) and (e2,y)
   * when e1 is a direct predecessor of e2 (in p). */
  private def dropDirectSucc(sndRcv: (Set[(Event,Msg)],Set[(Event, Msg)]), p: NPomset)
              : (Set[(Event,Msg)],Set[(Event, Msg)]) =
    if sndRcv._1.isEmpty then sndRcv else
      val (e1,x1) = sndRcv._1.head
      val r2 = for (e2,x2) <- sndRcv._2 if x2==x1 && p.realPred(e2)(e1) yield (e2,x2)
//      val (nSnd,nRcv) =
      if r2.size != sndRcv._2.size
      then dropDirectSucc((sndRcv._1-(e1->x1), r2), p)
      else
        val (s3,r3) = dropDirectSucc((sndRcv._1-(e1->x1), sndRcv._2), p)
        (s3+(e1->x1),r3)

  /** Given a set of sends [(e1,x),(e2,y)...],
   * finds all pairs (ei,ej) that are ordered, i.e., that ei<=ej. */
  private def orderedPairs(evs: Set[(Event, Msg)], p: NPomset): Set[((Event,Msg),(Event,Msg))] =
    for (e1,x)<-evs; (e2,y)<-evs if e1!=e2 && p.isPred(e1,e2) yield (e1->x, e2->y)

  /** Given a sends (e1,x) and a set of receives [(e2,y),...],
   * find all (e2,y) such that x==y   */
  private def findMatches(ev:(Event,Msg), evs: Set[(Event, Msg)]): Set[(Event,Msg)] =
    for (e2,y)<-evs if y==ev._2 yield (e2,y)


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

  /** All sends and receives are properly paired, dependencies are plausible. */
  def choreographic(p: NPomset): Option[Error] = ???
