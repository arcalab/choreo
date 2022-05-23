package choreo.api

import choreo.api.GlobalCtx.*
import choreo.npomsets.NPomset.Event
import choreo.syntax.Agent
import choreo.syntax.Choreo.{Action, In, Out}


/**
 * Contextual information for a protocol
 *
 * @param agentCtx context information for each agent
 */
case class GlobalCtx(
  agentCtx:Map[Agent,AgentCtx]
):
  lazy val ins  = this.agentCtx.flatMap(o=> o._2.optionByRecv.map(_._1))
  lazy val outs = this.agentCtx.flatMap(o=> o._2.optionBySend.map(_._1))
  lazy val messages = this.agentCtx.flatMap(o=>o._2.messages).toSet


object GlobalCtx:
  type EvidenceMap = Map[Event,String]

  /**
   * Contextual information of an agent
   * @param agent they agent
   * @param name its name
   * @param options contextual information for each option
   */
  case class AgentCtx(
    agent:Agent,
    name:String,
    options:List[AgentOptCtx]
  ):

    lazy val globalTVars:Map[String,Option[String]]  =
      (for
        o <- options
        (_,(n,mbt)) <- o.typeVariables.event2TVar
      yield
        o.name++n->mbt).toMap

    lazy val optionBySend: Map[Out,List[Int]] =
      val all =
        for
          (o,i) <- options.zipWithIndex
          out <- o.agentCom.sends.values.toList
        yield
          out->i
      val res = all.groupMap(_._1)(_._2)
      res

    lazy val optionByRecv: Map[In,List[Int]] =
      val all =
        for
          (o,i) <- options.zipWithIndex
          out <- o.agentCom.receives.values.toList
        yield
          out->i
      val res = all.groupMap(_._1)(_._2)
      res

    lazy val agentsOptBySend: Map[Out, List[(Event,Int)]] =
      val all =
        for
          (o,i) <- options.zipWithIndex
          (out,es) <- o.eventsBySend
          e <- es
        yield
          (out,e,i)
      val res = all.groupMap(_._1)(s=>(s._2,s._3))
      res

    lazy val messages = this.options.flatMap(_.messages)

  /**
   * Contextual information of an agent opt, used to create the agent protocol.
   * @param name agent option name
   * @param agent actual agent
   * @param agentCom communicaiton information
   * @param agentEvid pre and post evidence for each communication activity
   * @param paramName parameter name for each event
   * @param typeVariables type variables information
   * @param shared events shared with other options of the agent
   */
  case class AgentOptCtx(
    name:String,
    agent:Agent,
    agentCom:Com,
    agentEvid:Evid,
    paramName:Map[Event,String],
    typeVariables:TVarInfo,
    initialValues:Map[Event,String],
    shared:Set[In|Out],
    fork:Option[ForkInfo]
  ):
    lazy val events:List[Event]         = paramName.keys.toList.sorted
    lazy val paramList:List[String]     = events.map(e=>paramName(e))//paramName.values.toList//.sorted
    lazy val typeVarsList:List[String]  = events.map(e=>typeVariables.event2TVar(e)._1)
    lazy val typeVarTypeList:List[(String,Option[String])] =
      for e<- events yield typeVariables.event2TVar(e)
      //(for (e,(n,_)) <- typeVariables.event2TVar yield n).toList//.sorted
    lazy val initialVals:List[String] = events.map(e=>initialValues(e))
    lazy val eventsBySend:Map[Out,Iterable[Event]] =
      agentCom.sends.groupMap(s=>s._2)(_._1)
    lazy val eventsByRecv:Map[In,Iterable[Event]] =
      agentCom.receives.groupMap(s=>s._2)(_._1)
    lazy val eventsByAction:Map[In|Out,Iterable[Event]] =
      (agentCom.sends++agentCom.receives).groupMap(s=>s._2.asInstanceOf[In|Out])(_._1)

    lazy val messages =
      eventsBySend.keySet.map(o=>o.m) ++ eventsByRecv.keySet.map(o=>o.m)

  sealed trait SplitPoint
  case class SinglePoint(e:Event)         extends SplitPoint
  case class MultPoints(events:Set[Event]) extends SplitPoint

  /**
   * Information about a fork point
   * @param pre a map from events to required values before forking
   * @param post a list of branches each containing a map from events
   *             to changed values after forking into that branch
   */
  case class BranchInfo(pre:Map[Event,String],post:List[Map[Event,String]])

  /**
   * Information about a join point
   * @param pre a list for each branch joined map from events to required values before joining
   * @param post a map from events to new values after joining
   */
  case class JoinInfo(pre:List[Map[Event,String]],post:Map[Event,String])


  /**
   * Information about forks
   * @param fork a map from each fork point to its branching information
   * @param join a map from each join point and its merging information
   */
  case class ForkInfo(fork:Map[SplitPoint,BranchInfo],join:Map[SplitPoint,JoinInfo])

  //case class ForkInfo(e:Event,fork:Map[Event,List[BranchInfo]],join:Map[Event,List[BranchInfo]])

  /**
   * Communication information for an Agent: to whom it sends and receives
   * @param sends events in which sends and the actual out channel
   * @param receives events in which receives and the actual in channel
   */
  case class Com(sends:Map[Event,Out],receives:Map[Event,In])

  /**
   * Pre Evidence and Post evidence for each event
   * @param pre events to events the evidence it needs to execute
   * @param post events to the evidence it sets after executing
   */
  case class Evid(pre:Map[Event,EvidenceMap], post:Map[Event,EvidenceMap])

  /**
   * Information regarding type variables for an agent
   * @param event2TVar events with their corresponding type variable name
   * @param typ events with their corresponding type variable type
   */
  case class TVarInfo(event2TVar:Map[Event,(String,Option[String])]):
    def addVar(e:Event,n:String,t:Option[String]):TVarInfo =
      TVarInfo(event2TVar+(e->(n,t)))

