package choreo.api

import choreo.syntax.{Agent, Choreo}
import choreo.api.GlobalCtx
import choreo.api.GlobalCtx.*
import choreo.npomsets.{Choreo2NPom, NPomset}
import choreo.npomsets.NPomset.*
import choreo.syntax.Choreo.*

import java.util.concurrent.ForkJoinPool

object NPomGlobalCtx:

  // context information for a pomset
  def apply(npom:NPomset):GlobalCtx =
    // all options
    val choices = npom.refinements
    val pomsByAgent = for a <- npom.agents yield a-> choices.map(_.project(a))
    val agent2Ctx = for (a,poms) <- pomsByAgent yield a->apply(a,poms)
    GlobalCtx(agent2Ctx.toMap)

  // context information for an agent
  protected def apply(agent:Agent,poms:List[NPomset]):AgentCtx =
    val shared = sharedActions(poms).map(_.asInstanceOf[In|Out])
    val options =
      if poms.size == 1 then
        apply(agent,poms.head.simplifiedFull,shared,0)(true)::Nil
      else for
        (p,i)<-poms.zipWithIndex
        psim = p.simplifiedFull
      yield apply(agent,psim,shared,i)(false)
    AgentCtx(agent,agentClassName(agent),options)

  lazy val lastEvent = -1
  lazy val forked = -2

  // context information for an option
  protected def apply(agent:Agent, pom:NPomset, shared:Set[In|Out], index:Int)
    (implicit single:Boolean):AgentOptCtx =
    lazy val forkPoints = getForkPoints(pom)
    val canFork         = single && forkPoints.nonEmpty
    val name            = optionClassName(agent,index)
    val agentCom        = mkAgentCom(pom)
    val agentEvid       = mkAgentEvid(agent,pom)
    val eventNames      = getEventNames(pom)
    val paramNames      = mkParamNames(eventNames)
    val typeVariables   = mkTVariables(eventNames)
    val initialVals     = paramNames.map(e=>e._1->"true")
    val res             =
      AgentOptCtx(
        name,
        agent,
        agentCom,
        agentEvid,
        paramNames,
        typeVariables,
        initialVals,
        shared,
        None
      )
    if canFork then
      addForkInfo(res,mkForkInfo(forkPoints, pom))
    else res

  protected def addForkInfo(agentCtx:AgentOptCtx,forkInfo: ForkInfo):AgentOptCtx =
    val nParams   = agentCtx.paramName+(lastEvent->"e")+(forked->"f")
    val nTypeVar  = agentCtx.typeVariables.addVar(lastEvent,"E",Some("ET"))
      .addVar(forked,"F",Some("TF"))
    val ninitials = agentCtx.initialValues+
      (lastEvent->"false")+
      (forked->"false")
    val npost =
      for (e,ev) <- agentCtx.agentEvid.post yield
        e->(ev+(lastEvent->e.toString))
    val evid = Evid(agentCtx.agentEvid.pre,npost)
    AgentOptCtx(
      agentCtx.name,
      agentCtx.agent,
      agentCtx.agentCom,
      evid,
      nParams,
      nTypeVar,
      ninitials,
      agentCtx.shared,
      Some(forkInfo)
    )


  protected def mkForkInfo(forkPoints:Set[SplitPoint],pom:NPomset):ForkInfo =
    val branches = for fp <- forkPoints yield fp -> mkBranchInfo(fp,pom)
    val joins    = for jp <- getJoinPoints(pom) yield jp -> mkJoinInfo(jp,pom)
    //println(s"branches: $branches")
    //println(s"joins: $joins")
    ForkInfo(branches.toMap,joins.toMap)

  protected def  mkBranchInfo(forkPoint: SplitPoint,pom:NPomset):BranchInfo = forkPoint match
    case SinglePoint(e) =>
      val succ  = pom.succ.getOrElse(e,Set())
      val pre   = Map(e->"false") ++ succ.map(s=> s->"true")
      val post  = mkBranchPost(succ,pom)
      BranchInfo(pre,post)
    case MultPoints(events) =>
      val pre = events.map(e=>e->"true").toMap
      val post = mkBranchPost(events,pom)
      BranchInfo(pre,post)

    protected def mkBranchPost(events:Set[Event],pom:NPomset):List[Map[Event,String]] =
      (for
        e <- events
        others = events - e
        toFalse = others++others.flatMap(e=>pom.allSuccesors(e))
      yield
        toFalse.map(e=> e -> "false").toMap).toList

  protected def mkJoinInfo(jp:SplitPoint,pom:NPomset):JoinInfo = jp match
    case SinglePoint(e) =>
        val toFalse = pom.events.toSet.map(e=>e->"false").toMap
        val pred    = pom.realPred(e)
        val succ    = pom.allSuccesors(e)
        val pre     = for p <- pred yield toFalse+(lastEvent->e.toString)+(forked->"true")//+(forked->"true")
        val post    = for s <- succ yield s->"true"
        val post1   = (pom.allRealPred(e).map(e=> e->"false"))
        JoinInfo(pre.toList,
          post.toMap++post1+(lastEvent->"false")+(forked->"false"))
    case MultPoints(es) =>
      val toFalse = pom.events.toSet.map(e=>e->"false").toMap
      val pre = es.map(e=>toFalse+(lastEvent->e.toString)+(forked->"true"))
      val post = toFalse+(lastEvent->"false")+(forked->"false")//es.flatMap(e=>pom.allSuccesors(e)).map(s=>s->"true")+(forkEvent->"false")
      JoinInfo(pre.toList,post)



  protected def getForkPoints(pom:NPomset):Set[SplitPoint] =
    val forkEvents:Set[SplitPoint] =
      getForkEvents(pom).map(e=>SinglePoint(e))
    val initialFork = pom.minimum()
    if initialFork.size>1 then
      forkEvents + MultPoints(initialFork)
    else
      forkEvents

  protected def getJoinPoints(pom:NPomset):Set[SplitPoint] =
    val forkEvents:Set[SplitPoint] =
      getJoinEvents(pom).map(e=>SinglePoint(e))
    val lastJoin = for e<- pom.events.toSet; if pom.allSuccesors(e).isEmpty yield e
    if lastJoin.size>1 then
      forkEvents + MultPoints(lastJoin)
    else
      forkEvents

  protected def getForkEvents(pom:NPomset):Set[Event] =
    for
      e <- pom.events.toSet
      directSucc = pom.succ.getOrElse(e,Set())
      if directSucc.size>1
    yield
      e

  protected def getJoinEvents(pom:NPomset):Set[Event] =
    for
      e <- pom.events.toSet
      directPred = pom.realPred(e)
      if directPred.size>1
    yield
      e

  //type EventNames = Map[Event,(String,Option[Int])]
  type EventNames2 = Map[Event,String]

  protected def mkTVariables(eventNames: EventNames2):TVarInfo =
    val info = for (e,n) <- eventNames yield
        e->("V"++n,Some("TF"))
    //val types = names.view.mapValues(_=> "TF")
    TVarInfo(info)//,types.toMap)

  //protected def mkTVariables(eventNames: EventNames):TVarInfo =
  //  val info = for (e,(n,i)) <- eventNames yield
  //    if i.isDefined then
  //      e->("V"++n++i.get.toString,Some("TF"))
  //    else e->("V"++n,Some("TF"))
  //  //val types = names.view.mapValues(_=> "TF")
  //  TVarInfo(info)//,types.toMap)

  protected def getEventNames(pom: NPomset):EventNames2 =
    (for e<- pom.events.toSet yield e->e.toString).toMap
  //protected def getEventNames(pom: NPomset):EventNames =
  //  val com             = mkAgentCom(pom)
  //  val to              = com.sends.map(s=>s._1->s._2.b.s)
  //  val from            = com.receives.map(s=>s._1->s._2.b.s)
  //  val groupByPassive  = (to++from).groupBy(_._2)
  //  (for (b,m) <- groupByPassive yield
  //    if m.size > 1 then
  //      m.zipWithIndex.map({case ((e,b),i) => e -> (b,Some(i+1))}).toMap
  //    else
  //      Map[Event,(String,Option[Int])](m.head._1->(b,None))).flatten.toMap


  protected def mkParamNames(eventNames:EventNames2):Map[Event,String] =
    for (e,n) <- eventNames yield e->("v"++n)

  //protected def mkParamNames(eventNames:EventNames):Map[Event,String] =
  //  for (e,(n,i)) <- eventNames yield
  //    if i.isDefined then
  //      e->("v"++n++i.get.toString)
  //    else e->("v"++n)

  protected def mkAgentCom(pom:NPomset):Com =
    val sends = pom.actions.collect({case (e,a@Out(_,_,_)) => e->a})
    val recs  = pom.actions.collect({case (e,a@In(_,_,_)) => e->a})
    Com(sends,recs)

  protected def mkAgentEvid(a:Agent,pom:NPomset):Evid =
    var pre   = Map[Event,EvidenceMap]()
    var post  = Map[Event,EvidenceMap]()
    for e <- pom.events.toSet do
      pre += (e->mkPreEvid(e,pom))
      post += (e->mkPostEvid(e))
    Evid(pre,post)

  protected def mkPreEvid(e:Event,pom:NPomset):EvidenceMap =
    pom.realPred(e).map(pre => pre->"false").toMap + (e->"true")

  protected def mkPostEvid(e:Event):EvidenceMap =
    Map(e->"false")


  /* ---- HELPERS ---- */

  protected def optionClassName(a:Agent,i:Int): String = a.s.toUpperCase + i.toString
  protected def agentClassName(a:Agent): String = a.s.toUpperCase


  protected def sharedActions(poms:Iterable[NPomset]):Set[Action] =
    val actionsPerPom = poms.map(p=>p.actions.values.toSet)
    if actionsPerPom.size > 1 then
      actionsPerPom.tail.foldRight(actionsPerPom.head)(_.intersect(_))
    else Set()