package choreo.npomsets

import NPomset.*
import choreo.common.MRel
import choreo.common.MRel.*
import choreo.datastructures.Isomorphism.IsoResult
import choreo.realisability.{CC, CCNPOM, CCPOM, ICNPOM, ICPOM, Interclosure, Merge}
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.{DSL, Examples, Utils, npomsets}

import scala.annotation.targetName
import scala.collection.immutable.HashSet

// Idea: generalise actions to Act with a1 --> a2 (a2 depends on a1)
/**
 * Variation of the Pomset structure, using a nesting structure `N` that groups events.
 * It is not kept normalised, i.e., the mapping of `actions` and `order` may refer to
 * non-existing events in the `event` nested set.
 *
 * So far, ignoring loops and delayed choices.
 * @author José Proença
 */
case class NPomset(events: Events,   // identifiers (int) of events
                   actions: Actions, // sets of actions (with internal dependencies)
                   pred:Order,       // extra set of depedencies between events
                   loop:LoopInfo):   // dependencies with newly expendaded events
  lazy val agents:Set[Agent] =
    actions.flatMap(kv => Choreo.agents(kv._2)).toSet

  lazy val primitiveActions: Set[Action] = actions.values.toSet.flatMap(primitiveActions)
  private def primitiveActions(c:Choreo): Set[Action] = c match
    case Send(as, bs, m) =>
      (for (a<-as.toSet; b<-bs.toSet) yield Set(In(b,a,m),Out(a,b,m))).flatten
    case Seq(c1, c2) => primitiveActions(c1)++primitiveActions(c2)
    case Par(c1, c2) => primitiveActions(c1)++primitiveActions(c2)
    case Choice(c1, c2) => primitiveActions(c1)++primitiveActions(c2)
    case DChoice(c1, c2) => primitiveActions(c1)++primitiveActions(c2)
    case Loop(c) => primitiveActions(c)
    case Choreo.End => Set()
    case action: Action => Set(action)

  /** Remove an event from the NPomset */
  @targetName("deleteEvent")
  def -(e: Event): NPomset = this -- Set(e)
  /** Remove a set of events from the NPomset */
  @targetName("deleteEvents")
  def --(es:Set[Event]):NPomset =
//    NPomset(events--es,actions--es, pred, loop) // not dropping from the order, since it could break chains
  //            order.filterNot(o=>es.contains(o.left) || es.contains(o.right)))
    NPomset(events--es,actions--es,
      (pred--es).map(kv => kv._1->(kv._2--es)).filter(_._2.nonEmpty), // NOW dropping from the order
      loop) // not dropping from the order, since it could break chains

  /** Weak sequencing of NPomsets */
  @targetName("andThen")
  def >>(other:NPomset): NPomset =
    val deps =
      for a <- other.agents
          in <- actions.filter(p=>isActive(a,p._2)).keys
          inOther <- other.actions.filter(p=>isActive(a,p._2)).keys
      yield (inOther,in)
    NPomset(events++other.events,actions++other.actions,
      (pred :++ other.pred) :++ deps,
      addLoops(loop,other.loop))

  private def isActive(agent: Agent, act: Choreo): Boolean = act match
    case In(`agent`,_,_) => true
    case Out(`agent`,_,_) => true
    case Internal(`agent`,_) => true
    case Send(as,bs,m) => sys.error("Only atomic actions can be used when combining pomsets")
    case Seq(c1, c2) => isActive(agent,c1)
    case Par(c1, c2) => isActive(agent,c1) || isActive(agent,c2)
    case Choice(c1, c2) => isActive(agent,c1) || isActive(agent,c2)
    case DChoice(c1, c2) => isActive(agent,c1) || isActive(agent,c2)
    case Loop(c) => isActive(agent,c)
    case End => false
    case _ => false

  /** Choice of NPomsets (ignoring loops so far) */
  def or(other:NPomset): NPomset =
    NPomset(events or other.events, actions++other.actions,
      pred :++ other.pred, addLoops(loop,other.loop))

  /** Parallel composition of NPomsets */
  @targetName("joinPomsets")
  def ++(other: NPomset): NPomset =
    NPomset(events ++ other.events, actions++other.actions,
      pred :++ other.pred, addLoops(loop,other.loop))

  //  def refinements: Set[NPomset] =
  //    (for choice <- events.cs do NPomset(Nesting())

  def refinements:List[NPomset]=
    (for (rn<-events.refine) yield
      NPomset(rn,actions,pred,loop).simplifiedFull).toList
  def refinementsProj:List[List[NPomset]] =
    for r <- refinements yield
      agents.map(a=>r.project(a).simplifiedFull).toList
  ///////////////
  // Refinement functions to be used in the semantics
  ///////////////


  /** Do minimum refinement until the pomset is ready to perform `e` */
  def readyFor3(e:Event): Option[(NPomset,Event)] =
    //println(s"[ready] $e by $this")
    val preds = this.realPred(e)
//    println(s"realPreds($e)= $preds; pred($e)=${if pred contains e then pred(e) else "-"}")
    preds.headOption match
      case None =>
//        println(s"[done] $e by $this. Filtering.")
        //Some(this,e) // need to select!
        ///// SELECTING ////
        val res = select3(e,events)
//        val res = select2(evs=>evs.toSet contains e) // keep only choices that have `e` inside
//          .map(p=>(p,e))
//        println(s"[done] filtered: $res")
        res
      case Some(nxt) =>
        ///// FILTERING ////
        // keep only choices that do not have x at the top
        filterOut( nxt ) match // evs => !evs.acts.contains(nxt)) match
          case Some(newPom) => //println(s"filtered $nxt to $newPom. Recurse.");
            newPom.readyFor3(e)
          case None => //println(s"Could not filter out $nxt");
            None

  // drop choices from the nesting structure Events, and remove order in the end.
  def filterOut(e:Event): Option[NPomset] =
    for (toRemove,evs) <- filterOut(e,events) yield
      val res = NPomset(evs,actions,pred,loop)--toRemove
//      println(s"--- removing $toRemove from $evs --> $res --")
      res

  // keep only choices that obey the property f
  def filterOut(e:Event, evs:Events): Option[(Set[Event],Events)] =
    if evs.acts.contains(e) then None else
      var toRemove = Set[Event]()
      val newChoices:Set[Events] = for c<-evs.choices yield
        (filterOut(e,c.left),filterOut(e,c.right)) match
          case (None,None) => return None
          case (Some((as,a)),None) => toRemove++=as++c.right.toSet; a
          case (None,Some((bs,b))) => toRemove++=bs++c.left.toSet; b
          case (Some((as,a)),Some((bs,b))) => toRemove++=(as++bs); Nesting(Set(),Set(NChoice(a,b)),Set())
      for l<-evs.loops if l.toSet contains e do toRemove ++= l.toSet
      val newLoops: Set[Events] = evs.loops.filterNot(_.toSet.contains(e))
      val baseResult = Nesting(evs.acts,Set(),newLoops)
      val joinChoices = newChoices.fold(baseResult)(_++_)
      Some((toRemove,joinChoices))


  def select3(e:Event,evs1:Events): Option[(NPomset, Event)] =
    for
      // 2. if it is in a choice, remove alternatives.
      (evs2,genEvs,seed2) <- select(e,evs1,loop._2)
    yield
      val removed = evs1.toSet--evs2.toSet
      //println(s"Yeap. Got evs2 § genEvs: $evs2 § $genEvs ")
      val newActions = adaptActions(genEvs)--(evs1.toSet--evs2.toSet)
      val newPred    = adaptPred(genEvs)
      val realEvent  = genEvs.getOrElse(e,e)
      // return final pomset, with new actions, predecessors, and seed
      (npomsets.NPomset(evs2, (actions++newActions)--removed, pred++newPred, (loop._1,seed2)) , realEvent)

  def select2(f:Events=>Boolean): Option[NPomset] =
    for (toRemove,evs) <- select2(f,events) yield
      NPomset(evs,actions,pred,loop)--toRemove
  // when faced with an (exclusive) choice, if one matches the predicate remove the others
  def select2(f:Events=>Boolean, evs:Events): Option[(Set[Event],Events)] =
    if !f(evs) then None else
      var toRemove = Set[Event]()
      val newChoices: Set[Option[Events]] = for c<-evs.choices yield
        (f(c.left),f(c.right)) match
          case (true,false) => select2(f,c.left).map(x => {toRemove++=(x._1++c.right.toSet); x._2})
          case (false,true) => select2(f,c.right).map(x => {toRemove++=(x._1++c.left.toSet); x._2})
          case (false,false) => Some(Nesting(Set(),Set(c),Set()))
          case (true,true) => None
      val baseResult = Nesting(evs.acts,Set(),evs.loops)
      val joinChoices = newChoices.flatten.fold(baseResult)(_++_)
      Some((toRemove,joinChoices))








  /** Calculate the real predecessors of an event, skiping over elements of the order not in the NPomset */
  def realPred(e: Event): Set[Event] =
    pred.getOrElse(e,Set())
      .flatMap(e0=>if events.toSet contains e0 then Set(e0) else realPred(e0))

  /** Calculate ALL real predecessors of an event, skiping over elements of the order not in the NPomset */
  def allRealPred(e: Event): Set[Event] =
    val next = realPred(e)
    next ++ next.flatMap(allRealPred)

  /** Check if e1 <= e2 without calculating all predecessors if possible. */
  def isPred(e1:Event,e2:Event,hist:Set[Event]=Set()): Boolean =
    if e1 == e2 then true // found predecessor
    else if hist(e2) then false // failed to find predecessor
    else pred.getOrElse(e2,Set()).exists(e2b => isPred(e1,e2b,hist+e2)) // keep searching

  /** Removes predecessors with internal events.  */
  def reducedPred: Order =
    var newPred: Order = Map()
    for e <- events.toSet; ep <- realPred(e) do
      newPred = newPred :+ (e->ep) //add((e,ep),newPred)
    newPred

  def isExclusive(e1:Event, e2:Event): Boolean = isExclusive(e1,e2,events)

  private def isExclusive(e1:Event, e2:Event, ev:Events): Boolean =
    !ev.acts(e1) && !ev.acts(e2) && (
      ev.choices.exists(isExclusive(e1,e2,_) ||
      ev.loops.exists(isExclusive(e1,e2,_))))

  private def isExclusive(e1:Event, e2:Event, c:NChoice[Event]): Boolean =
    if      c.left.toSet(e1)  then c.right.toSet(e2) || isExclusive(e1,e2,c.left)
    else if c.right.toSet(e1) then c.left.toSet(e2)  || isExclusive(e1,e2,c.right)
    else false

  /** Removes predecessors that can be inferred via transitive closure. */
  def minimizedPred: Order =
    var newPred: Order = pred
    for
      (e,pes)<-pred
      pe<-pes
      if (pes-pe).flatMap(allRealPred) contains pe
    do
      newPred += e -> (newPred(e)-pe)
    newPred

  def minimized: NPomset = NPomset(events,actions,minimizedPred,loop)
  def reduced: NPomset = NPomset(events,actions,reducedPred,loop)
  def simplified: NPomset = this.reduced.minimized

  def simplifiedFull:NPomset =
    val s = this.simplified
    val es = events.toSet
    val npred = MRel.closure(es)(using s.pred)
    NPomset(s.events,
      actions.filter(kv=>es.contains(kv._1)),
      reduction(es)(using npred.filter(e=> es.contains(e._1)).map({case (e,p) => (e,p.intersect(es))})),
      s.loop)

  def simplifyChoices:NPomset =
    val (ne,nc):(Set[Events],Set[NChoice[Event]]) = getIsoChoices(this.events)
    NPomset(ne.foldRight[Events](Nesting(events.acts,nc,events.loops))(_++_),
      actions,pred,loop).simplifiedFull

  protected def getIsoChoices(n:Events): (Set[Nesting[Event]], Set[NChoice[Event]]) =
    var rm = Set[NChoice[Event]]()
    val nn = for c<-n.choices ; if canSimplify(c) yield {rm += c; c.left}
    (nn,n.choices--rm)

  protected def canSimplify(c:NChoice[Event]):Boolean =
    NPomIso.areIsomorphic(c.left,this,c.right,this).isDefined

  /**
   * Transitive closure of a an NPomset
   * @return Same NPomset with pred being the transitive closure
   */
  def closure: NPomset = NPomset(events,actions,MRel.closure(events.toSet)(using pred),loop)
  //var tc:Order = Map()
  //for e<-events.toSet do
  //  tc = visit(e,e,tc)
  //NPomset(events,actions,tc,loop)

  //protected def visit(from:Event,to:Event,cl:Order):Order =
  //  var tc = add((to,from),cl) //cl.updatedWith(to)(e => Some(e.getOrElse(Set())+from))
  //  for predec <- pred.get(from) ; e<-predec ; if !tc(to).contains(e) do
  //    tc = visit(e,from,tc)
  //  tc

  lazy val succ:Order = invert(using pred)

  def allSuccesors(e:Event):Set[Event] =
    var succesors = HashSet[Event]()
    var toVisit = HashSet(e)
    var visited = HashSet[Event]()
    while toVisit.nonEmpty do
      val next = toVisit.head
      val succesorsNext = succ.getOrElse(next,Set())
      toVisit ++= (succesorsNext -- visited)
      toVisit -= next
      visited += next
      succesors ++= succesorsNext
    succesors

  def minimum():Set[Event] =
    for e <- events.toSet ; if realPred(e).isEmpty yield e


  /** Do minimum refinement until the pomset is ready to perform `e` */
  @deprecated
  def readyFor(e:Event): Option[(NPomset,Event)] =
    println(s"checking if poms is ready for $e (real pred = ${allRealPred(e)}, dropEv = ${dropEvents(allRealPred(e),events)})")
    for
    // 1. for all predecessor, try to remove it by chosing empty choices (if available)
      evs1 <- dropEvents(allRealPred(e),events)
      // 2. if it is in a choice, remove alternatives.
      (evs2,genEvs,seed2) <- select(e,evs1,loop._2)
    yield
      println(s"Yeap. Got evs2 § genEvs: $evs2 § $genEvs ")
      val newActions = adaptActions(genEvs)
      val newPred    = adaptPred(genEvs)
      val realEvent  = genEvs.getOrElse(e,e)
      // return final pomset, with new actions, predecessors, and seed
      (npomsets.NPomset(evs2, actions++newActions, pred++newPred, (loop._1,seed2)) , realEvent)

  /** Go through the actions and, for every ev->act with ev a generator, ADD gen(nEv)->act */
  @deprecated
  def adaptActions(genEvs: Map[Event, Event]): Actions =
    for ((e,act)<-actions; nEv<-genEvs.get(e)) yield nEv->act

  /** Create new orders for the generated events: predecessors and sucessors (latter only in the loops)  */
  @deprecated
  def adaptPred(genEvs: Map[Event, Event]): Order =
    // 1. Generated vs. existing: go through the order and, for every e2<e1 and ADD e2<e1 and e2<gen(e1) (if exists)
    val newPred1: Order = for (e2,e1s) <- pred yield
      val newE1s: Set[Event] = for (e1<-e1s; ne1<-genEvs.get(e1)) yield ne1
      e2 -> (e1s ++ newE1s)
    // 1b. go through the order and  for every e2<e1 with e2 a generator, ADD gen(e2)<genEvs.getOrElse(e1,e1)
    val newPred1b:Order = for (e2,e1s) <- pred ; ge2 <- genEvs.get(e2) yield
      ge2 -> e1s.map(e1=>genEvs.getOrElse(e1,e1))
    // 2. Generated vs. Loops: go through the generated and, forall newE<-e, eDep<-loopInfo(e), ADD newE<eDep
    // fixed order: (e,newE) instead of (newE,e)
    val newPred2a:List[(Event,Event)] = for ((e,newE) <- genEvs.toList; eDep <- loop._1.getOrElse(e,Set())) yield
      eDep->newE
    val newPred2b:List[(Event,Event)] = for (e,newE) <- genEvs.toList yield
      e->newE
    val newPred2c = (newPred2a++newPred2b).foldLeft[Order]
      (Map()) ((m,pair) => m :+ pair)
    // 3. combine all sides
    newPred1b :++ (newPred1 :++ newPred2c)

  /** Refines (minimally) a nested set of events to drop a set of events.
   * Returne None if the events cannot be dropped. */
  @deprecated
  def dropEvents(es:Set[Event],n:Events): Option[Events] =
    if n.acts.intersect(es).nonEmpty then None
    else
      val newChoices:Set[Events] = for c <- n.choices yield
        (dropEvents(es,c.left),dropEvents(es,c.right)) match
          case (None,None) => return None // no need to continue
          case (Some(a),None) => a
          case (None,Some(b)) => b
          case (Some(a),Some(b)) => Nesting(Set(),Set(NChoice(a,b)),Set())
      val newLoops: Set[Events] = n.loops.filter(n => (n.toSet intersect es).isEmpty)

      val baseResult = Nesting(n.acts,Set(),newLoops)
      val joinChoices = newChoices.fold(baseResult)(_++_)
      Some(joinChoices)


  @deprecated
  def someIn(es:Set[Event],n:Events): Boolean = es.exists(n.acts) // nacts contains some es

  /** Tries to remove events `es` from a nesting structure `n`, dropping some choices and loops. */
  @deprecated
  def dropEvents2(es:Set[Event],n:Events): Events =
    val es2 = es -- n.acts // drop this layer's events
    val newChoices:Set[Events] = for c <- n.choices yield
      val c2l = dropEvents2(es2,c.left)
      val c2r = dropEvents2(es2,c.right)
      if someIn(es2,c2l) then c2r // failed to remove to c.left
      else if someIn(es2,c2r) then c2l // failed to remove to c.right
      else Nesting(Set(),Set(NChoice(c2l,c2r)),Set())
    val newLoops: Set[Events] = n.loops.filter(n => (n.toSet intersect es2).isEmpty)
    val baseResult = Nesting(n.acts,Set(),newLoops)
    newChoices.fold(baseResult)(_++_)


  /** Refines (minimally) a nested set of events to lift a given event up.
   * Returns None if the event is not found. */
  @deprecated
  def select(event: NPomset.Event,n:Events,seed:Event): Option[(Events,Map[Event,Event],Event)] =
    //println("sel")
    if n.acts.contains(event)
    then Some(n,Map(),seed)
    else
      // traversing choices and loops while upating "found,next,genEvs".
      var found = false
      var next = seed
      var genEvs: Map[Event,Event] = Map() // new generated events mapped from their originals
      // 1. Choices
      val newChoices = for c<-n.choices yield
        val (evs2,genEvs2,next2,found2) = selectChoice(event,c,next)
        found = found || found2
        next = next2
        genEvs ++= genEvs2
        evs2
      // 2. Loops
      //println("loops")
      val newLoops = for l<-n.loops yield
        //println(s"loop: ${l}")
        selectLoop(event,l,next) match
          case None => //println("none selected");
            Nesting(Set(),Set(),Set())
          case Some((evs2,genEvs2,next2)) =>
            //println(s"some $evs2 selected");
            found = true
            next = next2
            genEvs ++= genEvs2
            evs2
      // 3. compile result
      if !found then None
      else
        val jointEvents = (newChoices++newLoops).fold(Nesting(n.acts,Set(),n.loops))(_++_)
        Some(jointEvents, genEvs, next)

  @deprecated
  private def selectChoice(e:Event,c:NChoice[Event],seed:Event):
  (Events,Map[Event,Event],Event,Boolean) = // true if found
    select(e,c.left,seed) match
      case Some(es1,n1,s1) => (es1,n1,s1,true)
      case None => select(e,c.right,seed) match
        case Some(es2,n2,s2) => (es2,n2,s2,true)
        case None => (Nesting(Set(),Set(c),Set()),Map(),seed,false)

  @deprecated
  private def selectLoop(e:Event,loop:Events,seed:Event):
  Option[(Events,Map[Event,Event],Event)] = // true if found
    select(e,loop,seed) match
      case None => None
      case Some((es1,new1,s1)) =>
        var next = s1
        val newEvents = for e <- es1.toSet yield
          next += 1
          e -> next
        val nest2: Events = es1.map(newEvents.toMap.apply)
        val nest3 = Nesting(nest2.acts,nest2.choices,nest2.loops+loop)
        Some((nest3,new1++newEvents.toMap,next))


  ///////////////////

  def accepting: Boolean = canTerminate(events)
  private def canTerminate(es: Events): Boolean =
    es.acts.isEmpty && es.choices.forall(canTerminate)
  private def canTerminate(ch: NChoice[Event]): Boolean =
    canTerminate(ch.left) || canTerminate(ch.right)

  ///////////////////////

  def project(a:Agent):NPomset =
    val target = actions.filter(act => Choreo.agents(act._2) contains a).keySet
    val p = NPomset(project(target,events),actions.filter(x=>target contains x._1),pred,loop)//.simplified
    p

  def projectMap:Map[Agent,NPomset] =
    (for a <- agents yield a->project(a)).toMap

  def projectMapByChoice:Map[Agent,List[NPomset]] =
    val choices = this.refinements
    val res =
      for a <- agents.view.toList yield
        a -> (
          for c <- choices yield
            c.project(a).simplifiedFull
          )
    res.toMap

  def project(es:Set[Event],nest:Events): Events =
    Nesting(nest.acts.intersect(es),
      nest.choices.map( c => NChoice(project(es,c.left), project(es,c.right)) ),
      nest.loops.map(project(es,_)))

  def projectAll: Iterable[NPomset] =
    actions
      .values                    // all actions
      .flatMap(Choreo.agents(_)) // all agents
      .toSet
      .map(project)              // all projections

  def project(act: Action): NPomset =
    val target = actions.filter(a => a._2 == act).keySet
    //val np: Order = for (a, bs) <- pred
    //                    if actions.isDefinedAt(a) && actions(a) == act
    //                yield a -> bs.filter(e => actions.isDefinedAt(e) && actions(e) == act)
    val p= NPomset(project(target,events), actions.filter(k=>target.contains(k._1)),pred, loop).simplified
    p
  //NPomset(events, actions, np, loop).simplified

  ///////////////////////////

  def icnpom: List[Interclosure] = ICNPOM(this)
  //val pm = this.projectMap
  //(pm.values,Interclosure(pm))

  def icpom: List[Interclosure] = ICPOM(this)
  //def einterclosure:(Iterable[NPomset],List[Order]) =
  //  val pm = this.projectMap
  //  (pm.values,EInterclosure(pm))

  def mergeIC: NPomset = Merge.compose(icnpom.head)

  ////////////////////////////////////////////////////
  // Old experiments with wellBranchedness
  ////////////////////////////////////////////////////

  def wellBranched:Boolean =
    wellBranched(this.events)

  protected def wellBranched(n:Events):Boolean =
    n.choices.forall(wellBranched)

  protected def wellBranched(c:NChoice[Event]):Boolean =
    val il = init(c.left,this.pred)
    val ir = init(c.right,this.pred)
    //println(s"[wellBranched] - c = ${c}")
    //println(s"[wellBranched] - il = ${il}")
    //println(s"[wellBranched] - ir = ${ir}")
    //println(s"[wellBranched] - il(actions) == ir(actions) = ${il.map(actions).map(Choreo.agents(_))} == ${ir.map(actions).map(Choreo.agents(_))}")
    val ail = il.map(actions).map(Choreo.agents(_))
    val air = ir.map(actions).map(Choreo.agents(_))
    //il.size<=1 && ir.size<=1
    // todo: or if they are the same (by looking at lables)?
    ail.size<=1 && ail.size<=1 //only one agent initiates or none
      && ail == air  // they are the same agent
      && wellBranched(c.left) && wellBranched(c.right) // their nested choices are well branched


  //def realisable:Boolean = NPomRealisability(this)

  def cc2: CC.CCPomInfo = CCPOM.cc2(this)
  def cc3: CC.CCPomInfo = CCPOM.cc3(this)

  def cc2npom: CC.CCPomInfo = CCNPOM.cc2(this)
  //////////////////
  // Auxiliary
  //////////////////
  override def toString: String =
    //    val evs = events.toSet
    val sEv = events.show
    val sAct = actions.map((a,b)=>s"$a:$b").mkString(",")
    val sOrd = (for ((a,bs)<-reducedPred; b<-bs) yield s"$b<$a").mkString(",")
    val sLoop = (for ((a,bs)<-loop._1; b<-bs) yield s"$a'<$b").mkString(",")
    val sSeed = loop._2
    List(sEv,sAct,sOrd,sLoop,sSeed).filterNot(_=="").mkString(" | ")

  override def hashCode(): Event =
    (events,actions,reducedPred).hashCode()

  override def equals(obj: Any): Boolean = obj match
    case n: NPomset =>
      (n.events,n.actions,n.reducedPred) == (events,actions,reducedPred)
    case _ => false

  /** True if it has no events */
  def isEmpty: Boolean = events.toSet.isEmpty

  /* Experiments with merging */

object NPomset:
  type Event = Int
  type Actions = Map[Event,Choreo]
  type Order = MR[Event,Event] // Map[Event,Set[Event]]
  type Events = Nesting[Event]

//
//  /** MS[A,B] = Map[A,Set[B]] is isomorphic to Set[(A,B)], indexed on A */
//  type MS[A,B] = Map[A,Set[B]]
//  def mapset[A,B](ab:(A,B)) = Map(ab._1->Set(ab._2))
//  def mapset[A,B](abs:Iterable[(A,B)]) = add(abs,Map())
//  def add[A,B](ab:(A,B),m:MS[A,B]): MS[A,B] =
//    val (a,b) = ab
//    if m contains a then m+(a->(m(a)+b)) else m+(a->Set(b))
//  def add[A,B](abs:Iterable[(A,B)],m:MS[A,B]): MS[A,B] =
//    abs.foldRight(m)((ab,prev)=> add(ab,prev))
//  def add[A,B](m1:MS[A,B], m2:MS[A,B]): MS[A,B] =
//    m1 ++ (for (a,bs)<-m2 yield
//    if m1 contains a then a->(m1(a)++bs) else a->bs)
//  def invert[A,B](m:MS[A,B]):MS[B,A] =
//    var in:MS[B,A] = Map()
//    for ((a,bs) <- m; b<-bs)
//      in = add((b,a),in)
//    in
//
//  def toPair[A,B](o:MS[A,B]):Set[(A,B)] =
//    o.map({case (k,vs)=> vs.map(v=>(k,v))}).flatten.toSet
//
//  def closure[A](o:MS[A,A],es:Set[A]):MS[A,A] =
//    var tc:MS[A,A] = Map()
//    for e<-es do
//      tc = visit(e,e,tc,o)
//    //println(s"[closure] - of $o is:\n$tc")
//    tc
//
//  protected def visit[A](from:A,to:A,cl:MS[A,A],pred:MS[A,A]):MS[A,A] =
//    var tc = add((from,to),cl) //cl.updatedWith(to)(e => Some(e.getOrElse(Set())+from))
//    for predec <- pred.get(to) ; e<-predec ; if !tc(from).contains(e) do
//      tc = visit(from,e,tc,pred)
//    tc
//
//  def reduction[A](elems:Set[A],ms:MS[A,A]):MS[A,A] =
//    var reduced = ms.map({case (k,v)=> (k,v-k)}) // remove reflexive
//    def reachable(e:A):Set[A] =
//      if !reduced.isDefinedAt(e) then Set()
//      else reduced(e)++reduced(e).flatMap(e1=>reachable(e1))
//
//    for (e1<-elems;e2<-elems; if reduced.contains(e1) && reduced(e1).contains(e2))  // && e1!=e2)
//      reduced = reduced.updated(e1,reduced(e1)--reachable(e2))
//    reduced
//
//  def subTree[A](e:A, o:MS[A,A]):MS[A,A] =
//    var toVisit = o.getOrElse(e,Set())
//    var ch:MS[A,A] = Map(e->toVisit)
//    var visited = Set(e)
//    while toVisit.nonEmpty do
//      val n = toVisit.head
//      //toVisit -= n
//      if (!visited.contains(n)) then
//        val cn = o.getOrElse(n,Set())
//        toVisit ++= cn
//        ch += n->cn
//        visited+=n
//      toVisit-=n
//    ch
//
//  def subTree[A](es:Set[A], o:MS[A,A]):MS[A,A] =
//    var sub:MS[A,A] = es.map(e=>e->o.getOrElse(e,Set())).toMap
//    var toVisit:Set[A] = sub.flatMap(_._2).toSet
//    var visited = es
//    while toVisit.nonEmpty do
//      val n = toVisit.head
//      if (!visited.contains(n)) then
//        val cn = o.getOrElse(n,Set())
//        toVisit ++= cn
//        visited+=n
//        sub += n->cn
//      toVisit  -=n
//    sub





  /** Nested sets: with choices and loops structures that can be refined */
  case class Nesting[A](acts:Set[A], choices:Set[NChoice[A]],loops:Set[Nesting[A]]):
    lazy val toSet:Set[A] = acts ++ choices.flatMap(_.toSet) ++ loops.flatMap(_.toSet)
    @targetName("delete")
    def --(as:Set[A]):Nesting[A] = Nesting(acts--as,choices.map(_--as),loops)
    @targetName("join")
    def ++(other:Nesting[A]): Nesting[A] = Nesting(acts++other.acts,choices++other.choices,loops++other.loops)
    def or(other:Nesting[A]): Nesting[A] = Nesting(Set(),Set(NChoice(this,other)),Set())
    def map[B](f:A=>B):Nesting[B] = Nesting(acts.map(f),choices.map(_.map(f)),loops.map(_.map(f)))

    def refine:Set[Nesting[A]] =
      if choices.isEmpty then
        Set(this)
      else
        val noChoice = Nesting(acts,Set(),loops)
        val rs = for ch<-choices yield ch.left.refine ++ ch.right.refine
        val cp = Utils.crossProduct(rs.map(e=>e.toList).toList)
        cp.map(l=>l.foldRight[Nesting[A]](noChoice)(_++_)).toSet
          //yield noChoice++rn

    def show: String =
      (acts.map(_.toString).toList ++
        choices.map(c => s"[${c.left.show}+${c.right.show}]").toList ++
        loops.map(l => s"(${l.show})*"))
        .mkString(",")


  case class NChoice[A](left:Nesting[A],right:Nesting[A]):
    lazy val toSet:Set[A] = left.toSet ++ right.toSet
    def --(as:Set[A]):NChoice[A] = NChoice(left--as,right--as)
    def map[B](f:A=>B):NChoice[B] = NChoice(left.map(f),right.map(f))

    // events in the higher level of each option
    lazy val top:Set[A] = left.acts ++ right.acts

  /** Information needed to unfold loops: order between instances, and seed to generate events */
  type LoopInfo = (Order,Event) // inner order of loops and seed to generate events
  def join(l1:LoopInfo,l2:LoopInfo): LoopInfo = (l1._1++l2._1,l1._2 max l2._2)
  def loopInfo(e1:Event,e2:Event,seed:Event=0): LoopInfo = (mkMR(e1->e2),seed)
  def noLoopInfo: LoopInfo = (Map(),0)
  def addLoops(l1:LoopInfo,l2:LoopInfo): LoopInfo = (l1._1 :++ l2._1, l1._2 max l2._2)

  /* Aux to know if a choice is well branced */
  def init(n:Events,pred: Order):Set[Event] =
  //println(s"[init] - pred: $pred")
    for e<-n.toSet ; if !pred.isDefinedAt(e) || pred(e).intersect(n.toSet).isEmpty yield e

  def empty: NPomset = NPomset(Nesting(Set(),Set(),Set()),Map(),Map(),(Map(),0))

  val nex: Events = Nesting(Set(1,2,3),Set(NChoice(Nesting(Set(4,5),Set(),Set()),Nesting(Set(6,7),Set(),Set()))),Set()) // 1,2,3,[4,5+6,7]
  val pex: NPomset = NPomset(nex,Map(1->Out(Agent("a"),Agent("b")),4->In(Agent("b"),Agent("a"))), mkMR(2->1,4->3), (Map(),0))
  import choreo.Examples._
  val ex2: NPomset = Choreo2NPom(((a->d) + (b->d)) > (a->d))

  def getEx(e:String): NPomset = Choreo2NPom(Examples.examples2show.find(_._1==e)
    .getOrElse("",choreo.syntax.Choreo.End)._2)
  val ex3: NPomset = getEx("ex30")
  val ex4 = Choreo2NPom(DSL.loop((a!b)>(a!c))>(a!d))
  val ex5 = NPomDefSOS.next(ex4).tail.head._2
  val ex6 = Choreo2NPom(DSL.loop(a->b) > (a!c))


