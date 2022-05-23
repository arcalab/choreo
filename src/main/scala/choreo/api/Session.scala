package choreo.api

import choreo.api.GlobalCtx.{AgentCtx, AgentOptCtx, BranchInfo, EvidenceMap, ForkInfo, JoinInfo, SplitPoint, TVarInfo}
import choreo.api.LocalAPI.{SetAPI, SingleAPI}
import choreo.api.MiniScala.*
import choreo.npomsets.{Choreo2NPom, NPomset}
import choreo.npomsets.NPomset.*
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.syntax.Choreo.{Choice, DChoice, In, Loop, Out, Par, Seq, agents, messages}
import choreo.api.NPomGlobalCtx
import choreo.api.Session.ScalaModule
import jdk.dynalink.linker.MethodHandleTransformer

import java.io.{File, FileWriter}
/**
 * Created by guillecledou on 24/02/2022
 *
 * Scala API Session
 */

case class Session(
  modules:List[ScalaModule]
  //localAPIs: List[LocalAPI],
  //modules:Map[String,Statement]
):

  def saveModules(path:String):Unit =
    for m<-modules do
      save(path++"/"++m.name++".scala",m.statements.toString)

  protected def save(path:String,str:String):Unit =
    val fileWriter = new FileWriter(new File(path))
    fileWriter.write(str)//this.toString)
    fileWriter.close()

  def modulesToCode:List[(String,String)] =
    modules.map(m=> (m.name,m.toString))
    //localAPIs.map(a=> (a.set.co.name,a.toString))
    //  ++ modules.map(m=>(m._1,m._2.toString)) :+ ("SessionUtils",utils)

  override def toString: String =
    modules.map(m=>m.toString).mkString("\n\n")

    //(localAPIs.map(_.toString)++
    //  modules.map(m=>m._2.toString)).mkString("\n\n") ++ "\n\n" ++ utils


object Session:

  case class ScalaModule(name:String, statements:Statement) extends Code:
    def toCode(implicit i: Int): String =
      statements.toCode

  def apply(npom:NPomset):Session =
    val ctx       = NPomGlobalCtx(npom)
    val localAPIs = for (a,agentCtx) <- ctx.agentCtx yield AgentAPI(agentCtx)
    val extras = mkRoles(ctx)::mkNetwork(ctx)::mkProtocol(ctx)::mkMsgs(ctx)::mkUtils()::Nil
    Session(localAPIs.toList++extras)//modules)

  def mkUtils():ScalaModule =
    val code =
      s"""object SessionUtils:
         |  type Channel = java.util.concurrent.ConcurrentLinkedQueue[Any]
         |
         |  def out(c: Channel, v: Any) = c.offer(v)
         |
         |  def in(ccc: Channel*) =
         |    var (i, c, v) = (0, ccc(0), ccc(0).poll())
         |    while v == null do
         |      i = (i + 1) % ccc.size
         |      c = ccc(i)
         |      v = ccc(i).poll()
         |    (c, v)
         |
         |  trait UseOnce:
         |    var used = false
         |    def use = if used then throw new Exception() else used = true
         |
         |  type TF = true | false
         |""".stripMargin
    ScalaModule("SessionUtils",PreCode(code))

  def mkRoles(globalCtx: GlobalCtx):ScalaModule =
    val agents = globalCtx.agentCtx.map(o=>o._1)
    var sts = List[Statement]()
    for a <- agents do
      sts ++= ScalaObject(roleName(a),None,None)::
        TypeDef(TName(roleName(a)),TName(roleName(a)++".type"))::Nil
    val st = MethodSts(sts)
    ScalaModule("Roles",ScalaObject("Roles",Some(st),None))

  def mkNetwork(globalCtx: GlobalCtx):ScalaModule =
    val imp = Import("SessionUtils.*")
    val chs = (globalCtx.ins ++ globalCtx.outs).map(i=>chName(i.asInstanceOf[In|Out]))
    val sts = for c <- chs.toSet yield VarDef(s"val $c",None,"new Channel")
    ScalaModule("Network",
      ScalaClass("Network",List(),Nil,
        Some(Statements(imp::MethodSts(sts.toList)::Nil)),None,Nil,false))

  def mkMsgs(globalCtx: GlobalCtx):ScalaModule =
    val sts =
      for
        m <- globalCtx.messages
      yield
        ScalaClass(msgName(m),List(),Nil,None,None,Nil,false)
    val st = MethodSts(sts.toList)
    ScalaModule("Messages",ScalaObject("Messages",Some(st),None))

  def mkProtocol(globalCtx: GlobalCtx):ScalaModule =
    val imports =
      Import("scala.annotation.targetName")::
        Import("SessionUtils.*")::Nil ++
        (for n <- globalCtx.agentCtx.values.map(a=>a.name) yield
          Import(n++".*")).toList
    val net = VarDef("val net",None,"new Network()")
    val runObjs = mkRunObjects(globalCtx)
    val runs = mkRuns(globalCtx)
    val st = Statements(MethodSts(imports)::net::Nil++runObjs++runs)
    ScalaModule("Protocol",ScalaClass("Protocol",List(),Nil,Some(st),None,Nil,false))

  def mkRuns(ctx: GlobalCtx):List[Statement] =
    for a <- ctx.agentCtx.keys.toList yield
      ScalaObject(s"Run${a.s.toUpperCase}",None,None,"UseOnce"::Nil)

  def mkRunObjects(ctx: GlobalCtx):List[Statement] =
    for a <- ctx.agentCtx.values.toList yield
      val name = s"run${a.name}"
      val params = mkRunParam(a)::Nil
      val st = mkRunSt(a)
      MethodDef("run",Nil,params,Set(),None,st,None,Some(s"""@targetName("$name")"""))

  def mkRunSt(agentCtx: AgentCtx):Statement =
    MethodSts(
      Variable(s"Run${agentCtx.name}.use")::
        VarDef("val thread",None,s"""new Thread(() => { f(${agentCtx.name}.start(net)); () })""")::
        FunCall("thread.start",Nil,Nil)::Nil
    )

  def mkRunParam(agentCtx: AgentCtx):Param =
    val from = TName(agentCtx.name++".Init")
    val to = TName(agentCtx.name++".Final")
    Param("f",TFun(from,to))

  /**
   * Builds a local API based on the context information
   */
  object AgentAPI:
    def apply(agentCtx:AgentCtx):ScalaModule = //:LocalAPI =
      val optionsAPIs =
        for opt <- agentCtx.options yield
          mkOptionsAPI(opt)
      val setAPI = mkSetAPI(agentCtx)
      val optSts = optionsAPIs.flatMap(o=>o.co::o.clas::Nil)
      //LocalAPI(setAPI,optionsAPIs)
      ScalaModule(agentCtx.name,
        Statements(
          Import(agentCtx.name++".*")::
          setAPI::Nil++optSts
        )
      )
    protected def mkFT(ctx:AgentCtx):Option[TypeDef] =
      if ctx.options.size == 1 && ctx.options.head.fork.isDefined then
        val events = ctx.options.head.events.toSet -- Set(-1,-2)
        Some(TypeDef(TName("ET"),TUnion("false"::events.toList.sorted.map(_.toString))))
      else None

    protected def mkOptionsAPI(agentCtx: AgentOptCtx):SingleAPI =
      val clas = mkOptionClass(agentCtx)
      val co = mkOptionCO(agentCtx)
      SingleAPI(clas,co)


    protected def mkSetAPI(agentCtx: AgentCtx):ScalaObject =
      val start = mkGlobalStart(agentCtx)
      val extension = mkExtension(agentCtx)
      val ft = mkFT(agentCtx)
      val join = mkJoins(agentCtx)
      val net = Param("net",TName("Network"))
      //var imports = for opt <- agentCtx.options yield Import(opt.name++".*")
      //imports ++= Import(agentCtx.name++".*")::
      //  Import("Roles.*")::
      //  Import("SessionUtils.*")::
      //  Import("Messages.*")::Nil
      //val impSt = MethodSts(imports)
      //val st = Statements(impSt::start::extension::join)
      val st = Statements(
        MethodSts(
          mkInitType(agentCtx)
            ::mkFinalType(agentCtx)
            ::Nil
            ++(if ft.isDefined then ft.get::Nil else Nil)
        )::MethodSts(
          VarDef("protected var _network",None,"new Network")
          ::VarDef("protected lazy val net",None, "_network")
            ::Nil)
          ::/*impSt::*/start::extension::join)
      //val co = ScalaObject(
      //  agentCtx.name,
      //  Some(MethodSts(mkInitType(agentCtx)::
      //    mkFinalType(agentCtx)::
      //    Nil++(if ft.isDefined then ft.get::Nil else Nil))),
      //  None
      //)
      //val magic = ScalaClass(agentCtx.name,Nil,net::Nil,Some(st),None,Nil,false)
      val setApi = ScalaObject(agentCtx.name,Some(st),None)
      setApi
      //SetAPI(co,magic)

    protected def mkFinalType(agentCtx: AgentCtx):TypeDef =
      val finals =
        for o <- agentCtx.options yield
          TName(o.name,Some(o.typeVarsList.map(_=>"false")))
      TypeDef(TName("Final"), if finals.size>1 then TTuple(finals) else finals.head)
    protected def mkInitType(agentCtx: AgentCtx):TypeDef =
      val inits =
        for o <- agentCtx.options yield
          TName(o.name,Some(o.initialVals))
      TypeDef(TName("Init"), if inits.size>1 then TTuple(inits) else inits.head)

    protected def mkExtension(agentCtx: AgentCtx):Extension =
      val typeVars = agentCtx.globalTVars
      val param =
        Param("p", TTuple(for o <- agentCtx.options yield
          TName(o.name,Some(o.typeVarsList.map(t=>o.name+t)))))
      val methods = mkExtMethods(agentCtx)
      val fork = mkGlobalFork(agentCtx)
      val ended   = mkEnded(typeVars.keys.toList)
      val sts = if fork.isDefined then methods :+ fork.get else methods
      Extension(typeVars,param,sts:+ended)

    protected def mkExtMethods(agentCtx: AgentCtx):List[MethodDef] =
      val sends = mkSends(agentCtx)
      val recvs = mkRecv(agentCtx)
      if recvs.isDefined then
        sends:+recvs.get
      else sends

    protected def mkGlobalFork(agentCtx: AgentCtx):Option[MethodDef] =
      val opt = agentCtx.options.head
      if agentCtx.options.size == 1 && opt.fork.isDefined then
        val st = FunCall("p.fork",Nil,Nil)
        Some(MethodDef("fork",Nil,Nil,Set(),None,st,None))
      else None

    protected def mkJoins(agentCtx: AgentCtx):List[Statement] =
      val opt = agentCtx.options.head
      if agentCtx.options.size == 1 && opt.fork.isDefined then
        val res = for (jp,jinfo) <- opt.fork.get.join yield
          mkJoin(jp,jinfo,opt)
        res.toList
      else
        Nil

    protected def mkJoin(jp:SplitPoint,ji:JoinInfo,agentCtx: AgentOptCtx):Statement =
      val arg = for p <- ji.pre yield TName(agentCtx.name,Some(mkOutTArgs(p,agentCtx)))
      val param = Param("j",if arg.length>1 then TTuple(arg) else arg.head)
      val outTArgs = mkOutTArgs(ji.post,agentCtx)
      val typ = TName(agentCtx.name,Some(outTArgs))
      val st = FunCall(s"new ${agentCtx.name}",Nil,outTArgs)

      val join = MethodDef("join",Nil,Nil,Set(),Some(typ),st,None)

      Extension(Map(),param,join::Nil)

    protected def mkSends(agentCtx: AgentCtx):List[MethodDef] =
      val eventsBySend = agentCtx.optionBySend
      val res = for (o,options) <- eventsBySend yield mkSend(o,options.toSet,agentCtx)
      res.toList

    protected def mkRecv(agentCtx: AgentCtx):Option[MethodDef] =
      val optByRecv = agentCtx.optionByRecv.map(e=>e._1->e._2.toSet)
      if optByRecv.size>=1 then
        val params = mkRecvParams(optByRecv,agentCtx)
        val cases  = mkRecvCases(optByRecv,agentCtx)
        val ins = optByRecv.keys.toList
        val receive = FunCall("in",Nil,ins.map(in=>"net."++chName(in)).distinct)
        val st = Match(receive.toString::Nil,cases)
        val ev = for (i,options) <-optByRecv yield mkSetMethodEvid(i,options,agentCtx)
        Some(MethodDef("recv",Nil,params,ev.flatten.toSet,None,st,None))
      else None

    protected def mkRecvCases(ins:Map[In,Set[Int]],agentCtx: AgentCtx):List[Case] =
      for ((i,options),idx)<- ins.zipWithIndex.toList
        yield mkRecvCase(i,options,agentCtx)(idx)

    protected def mkRecvCase(in:In,options:Set[Int],agentCtx: AgentCtx)(implicit i:Int):Case =
      val pattern = s"net.${chName(in)}"::s"m:${msgName(in.m)}"::Nil
      val recvCalls = mkMethodLastSt(in,options,agentCtx)
      val params = roleName(in.b)::"m"::recvCalls.toString::Nil
      val st = FunCall(s"f$i",Nil,params)
      Case(pattern,Nil,st)

    protected def mkRecvParams(ins:Map[In,Set[Int]],agentCtx: AgentCtx):List[Param] =
      for ((i,options),idx)<- ins.zipWithIndex.toList yield
        mkRecvParam(i,options,agentCtx)(idx)

    protected def mkRecvParam(in:In,options:Set[Int],agentCtx: AgentCtx)(implicit i:Int):Param =
      val roleType = TName(roleName(in.b))
      val msgType = TName(msgName(in.m))
      val retType = mkMethodType(in,options,agentCtx)
      val finalType = TName("Final",None)
      val pt = TFun(TTuple(roleType::msgType::retType::Nil),finalType)//retType)
      Param(s"f$i",pt)


    protected def mkMethodType(act:In|Out,options:Set[Int],agentCtx: AgentCtx):TExp =
      val params =
        for (o,i) <- agentCtx.options.zipWithIndex yield
          if options.contains(i) then
            mkMethodType(act,o.eventsByAction(act).toList,i,agentCtx)
          else
            TName(o.name,Some(o.typeVarsList.map(_=>"false")))
      if params.size== 1 then
        params.head
      else
        TTuple(params)

    protected def mkMethodType(act:In|Out,events:List[Event],i:Int,agentCtx:AgentCtx):TExp = events match
      case e::Nil =>
        val post = agentCtx.options(i).agentEvid.post(e)
        TName(agentCtx.options(i).name,
          Some(mkOutTArgs(post,agentCtx.options(i),agentCtx.options(i).name)))//.map(t=>agentCtx.options(i).name++t)))
      case _ =>
        val tvars = agentCtx.options(i).typeVarsList.map(t=>agentCtx.options(i).name++t)
        TName(agentCtx.options(i).name++matchTypeName(act),Some(tvars))



    protected def mkSend(o:Out,options:Set[Int],agentCtx: AgentCtx):MethodDef =
      val to = Param("to",TName(roleName(o.b)))
      val msg = Param("m",TName(msgName(o.m)))
      val ev = mkSetMethodEvid(o,options,agentCtx)
      val st = mkSendSt(o,options,agentCtx)
      MethodDef(methodName(o),Nil,to::msg::Nil,ev,None,st,None,None)


    protected def mkSendSt(o:Out,options:Set[Int],agentCtx: AgentCtx):Statement =
    MethodSts(
          FunCall("out",Nil,"net."++chName(o)::"m"::Nil)::
          mkMethodLastSt(o,options,agentCtx)::Nil
      )

    protected def mkMethodLastSt(a:In|Out,options:Set[Int],agentCtx: AgentCtx):Statement =
      val funName = methodName(a)
      val pName = a match
        case i:In => roleName(i.b)
        case i:Out => roleName(i.b)
      if agentCtx.options.size == 1 then
        FunCall(s"p."++funName,Nil,pName::"m"::Nil)
      else
        val args =
          for
            (opt,i) <- agentCtx.options.zipWithIndex
          yield
            if options.contains(i) then
              FunCall(s"p._${i + 1}.$funName",Nil,pName::"m"::Nil)
            else
              FunCall(s"p._${i+1}.end",Nil,Nil)
        Tuple(args)

    protected def mkSetMethodEvid(act:In|Out,options:Set[Int],agentCtx: AgentCtx):Set[Evidence] =
      //(evidences:List[EvidenceMap],agentCtx: AgentOptCtx):Set[Evidence]
      val agentOptCtxs = options.map(i=>agentCtx.options(i))
      val res =
        for
          opt <- agentOptCtxs
          es = opt.eventsByAction(act)
          evid = es.map(e=>opt.agentEvid.pre(e))
        yield
          mkEvidence(evid.toList,opt,opt.name)
      res.flatten.toSet


    //protected def mkMethods(agentCtx: AgentOptCtx):List[MethodDef] =
    //  // each action to the events that have that action
    //  val sends:Map[Event,In|Out] = agentCtx.agentCom.sends
    //  val recs:Map[Event,In|Out]  = agentCtx.agentCom.receives
    //  val actions = sends++recs
    //  val eventsByAction  = actions.groupMap(_._2)(_._1)
    //  val methods      = for (a,es) <- eventsByAction yield mkMethod(a,es.toList,agentCtx)
    //  methods.toList

    //protected def mkSends(agentCtx: AgentOptCtx):Statement =
    //  // each output action to the events that have that action
    //  val eventsBySend  = agentCtx.agentCom.sends.groupMap(_._2)(_._1)
    //  val sends         = for (o,es) <- eventsBySend yield mkSend(o,es,agentCtx)
    //  Statements(sends)

    def mkEnded(typVars:List[String]):MethodDef =
      val ev = Evidence(typVars.map(t=>t->"false").toMap)
      MethodDef("end",Nil,Nil,Set(ev),None,Variable("p"),None)

    protected def mkGlobalStart(agentCtx: AgentCtx):MethodDef =
      val starts = for o <- agentCtx.options yield o.name++".start()"
      val st = MethodSts(
        Asign("_network","network")
          ::FunCall("",Nil,starts)
          ::Nil
      )
      MethodDef("start",Nil,Param("network",TName("Network"))::Nil,Set(),None,st,None)

    protected def mkOptionClass(agentCtx: AgentOptCtx):ScalaClass =
      val typeVariables = agentCtx.typeVarTypeList
      val parameters    = mkParameters(agentCtx)
      val statements    = mkStatements(agentCtx)
      ScalaClass(
        agentCtx.name,
        typeVariables,
        parameters,
        Some(statements),
        None,
        "UseOnce"::Nil
      )

    protected def mkParameters(agentCtx: AgentOptCtx):List[Param] =
      val ev2tv = agentCtx.paramList.zip(agentCtx.typeVarTypeList)
      for (p,(n,t)) <- ev2tv yield
        Param(p,TName(n))

    //
    //protected def mkTExp(mbType: Option[String]):TExp = mbType match
    //  case Some(name) => TName(name,None)
    //  case None => TName("Any",None)

    protected def mkStatements(agentCtx: AgentOptCtx):Statement =
      val methods = mkMethods(agentCtx)
      val end  = mkEnd(agentCtx)
      val fork = mkFork(agentCtx)
      val sts = if fork.isDefined then methods :+ fork.get else methods
      Statements(sts:+end)


    protected def mkMethods(agentCtx: AgentOptCtx):List[MethodDef] =
      // each action to the events that have that action
      val sends:Map[Event,In|Out] = agentCtx.agentCom.sends
      val recs:Map[Event,In|Out]  = agentCtx.agentCom.receives
      val actions = sends++recs
      val eventsByAction  = actions.groupMap(_._2)(_._1)
      val methods      = for (a,es) <- eventsByAction yield mkMethod(a,es.toList,agentCtx)
      methods.toList

    //protected def mkSends(agentCtx: AgentOptCtx):Statement =
    //  // each output action to the events that have that action
    //  val eventsBySend  = agentCtx.agentCom.sends.groupMap(_._2)(_._1)
    //  val sends         = for (o,es) <- eventsBySend yield mkSend(o,es,agentCtx)
    //  Statements(sends)
    //
    //protected def mkSend(out:Out,events:List[Event],agentCtx: AgentOptCtx):MethodDef =
    //  val to  = Param("to",TName(roleName(out.b),None))
    //  val msg = Param("msg",TName(msgName(out.m),None))
    //  events match
    //    // single statement
    //    case e::Nil =>
    //      val st  = mkSingleSend(e,out,agentCtx)
    //      MethodDef("send",Nil,to::msg::Nil,Set(),None,st,None,None)
    //    // match statement
    //    case _  =>
    //      val typ = TName(matchTypeName(out),agentCtx.typeVarsList)
    //      val st  = mkMultSt(events,out,agentCtx)
    //      MethodDef("send",Nil,to::msg::Nil,Set(),typ,st,None,None)


    protected def mkCase(evid:EvidenceMap, st:Statement,agentCtx: AgentOptCtx):Case =
      //val evi = agentCtx.agentEvid.pre(e)
      val pattern =
        for e <- agentCtx.events yield
          if evid.isDefinedAt(e) then
            evid(e)
          else "_"
      Case(pattern,pattern,st)

    protected def mkEndCase(act:In|Out,agentCtx: AgentOptCtx):Case =
      val pattern = for e <- agentCtx.events yield "_"
      val st = FunCall("end",Nil,Nil)
      Case(pattern,if pattern.length == 1 then "false"::Nil else pattern,st)

    //protected def mkSingleSend(e:Event,out:Out,agentCtx: AgentOptCtx):Statement =
    //  val outArgs = mkOutArgs(e,agentCtx)
    //  FunCall(agentCtx.name,Nil,outArgs)
    //  // todo:  this goes glabaly
    //  //Statements(
    //  //  Variable("use")::
    //  //    FunCall("out",Nil,"net."++chName(out)::"m"::Nil)::
    //  //    FunCall(agentCtx.name,Nil,outArgs)::Nil
    //  //)

    protected def mkOutArgs(evid:EvidenceMap,agentCtx: AgentOptCtx):List[String] =
      //val evid = agentCtx.agentEvid.post(e)
      for event <- agentCtx.events yield
        if evid.contains(event) then
          evid(event)
        else agentCtx.paramName(event)

    //protected def mkRecvs(agentCtx: AgentOptCtx):Statement =
    //  // each input action to the events that have that action
    //  val eventsByRecv  = agentCtx.agentCom.receives.groupMap(_._2)(_._1)
    //  val receives      = for (i,es) <- eventsByRecv yield mkRecv(i,es,agentCtx)
    //  Statements(receives)

    //protected def mkRecv(in:In,events:List[Event],agentCtx: AgentOptCtx):MethodDef =
    //  val from  = Param("from",TName(roleName(in.b),None))
    //  val msg = Param("msg",TName(msgName(in.m),None))
    //  events match
    //    // single statement
    //    case e::Nil =>
    //      val st  = mkSingleSt(e,in,agentCtx)
    //      MethodDef("recv",Nil,from::msg::Nil,Set(),None,st,None,None)
    //    // match statement
    //    case _  =>
    //      val typ = TName(matchTypeName(in),agentCtx.typeVarsList)
    //      val st  = mkMultRecv(events,in,agentCtx)
    //      MethodDef("recv",Nil,from::msg::Nil,Set(),typ,st,None,None)

    //.map(t=>agentCtx.options(i).name++t)))
    //case _ =>
    //  val tvars = agentCtx.options(i).typeVarsList.map(t=>agentCtx.name++t)
    //  TName(agentCtx.options(i).name++matchTypeName(act),Some(tvars))

    protected def mkMethod(act:In|Out,events:List[Event],agentCtx: AgentOptCtx):MethodDef =
      val params = mkParams(act)
      events match
        // single statement
        case e::Nil =>
          val st  = mkSingleSt(e,act,agentCtx)
          val typ = TName(agentCtx.name,
            Some(mkOutTArgs(agentCtx.agentEvid.post(e),agentCtx)))
          MethodDef(methodName(act),Nil,params,Set(),Some(typ),st,None,None)
        // match statement
        case _  =>
          val typ = TName(agentCtx.name++matchTypeName(act),Some(agentCtx.typeVarsList))
          val st  = mkMultSt(events,act,agentCtx)
          MethodDef(methodName(act),Nil,params,Set(),Some(typ),st,None,None)

    //protected def mkMethodWithForkInfo(act:Int|Out, events: List[Event], agentCtx:AgentOptCtx):MethodDef =
    //  val params = mkParams(act)
    //  val typ = TName(agentCtx.name++matchTypeName(act),Some(agentCtx.typeVarsList))
    //  val st  = mkMultStWithFork(events,act,agentCtx)
    //  MethodDef(methodName(act),Nil,params,Set(),Some(typ),st,None,None)



    protected def mkParams(act:In|Out):List[Param] =
      val (roleNm,msgNm) = act match
        case i:In => (roleName(i.b),msgName(i.m))
        case o:Out => (roleName(o.b),msgName(o.m))
      val role = Param(paramName(act),TName(roleNm,None))
      val msg  = Param("m",TName(msgNm,None))
      role::msg::Nil


    protected def mkSingleSt(e:Event,act:In|Out,agentCtx: AgentOptCtx):Statement =
      val outArgs = mkOutArgs(agentCtx.agentEvid.post(e),agentCtx)
      MethodSts(
        Variable("use")::
          FunCall(agentCtx.name,Nil,outArgs)::
          Nil
      )

    protected def mkMultSt(es:List[Event],act:In|Out,agentCtx: AgentOptCtx):Statement =
      var cases = for e <- es yield
        mkCase(agentCtx.agentEvid.pre(e),mkSingleSt(e,act,agentCtx),agentCtx)
      if agentCtx.shared.contains(act) then
        cases :+= mkEndCase(act,agentCtx)
      Match(agentCtx.paramList,cases)

    protected def mkEnd(agentCtx: AgentOptCtx):Statement =
      val variablesVal = agentCtx.typeVarsList.map(_=>"false")
      val st = FunCall(s"new ${agentCtx.name}",Nil,variablesVal)
      val typ = TName(agentCtx.name,Some(variablesVal))
      MethodDef("end",Nil,Nil,Set(),None,st,None)
    //
    //protected def mkForkType(fork:ForkInfo,agentCtx: AgentOptCtx):TExp =
    //  for ev <- fork.fork
    //
    //  ???


    protected def mkFork(agentCtx: AgentOptCtx):Option[Statement] = agentCtx.fork match
      case Some(info) => info.fork.toList match
        case (sp,bi)::Nil =>
          val st  = mkSingleStFork(sp,bi,agentCtx)
          val typargs = for b <- bi.post yield mkOutTArgs(b,agentCtx)
          val typ = TTuple(
            for b <- typargs yield TName(agentCtx.name,Some(b))
          )
          Some(MethodDef("fork",Nil,Nil,Set(),Some(typ),st,None,None))
        case l =>
          val ev = mkEvidence(l.map(_._2.pre),agentCtx)
          val st = mkMultStFork(l,agentCtx)
          val typ = TName("Fork",Some(agentCtx.typeVarsList))
          Some(MethodDef("fork",Nil,Nil,ev,Some(typ),st,None,None))
      case None => None

    protected def mkMultStFork(forks:List[(SplitPoint,BranchInfo)],agentCtx: AgentOptCtx):Statement =
      var cases = for (s,bi) <- forks yield
        mkCase(bi.pre,mkSingleStFork(s,bi,agentCtx),agentCtx)
      Match(agentCtx.paramList,cases)

    protected def mkSingleStFork(sp:SplitPoint,bi:BranchInfo,agentCtx: AgentOptCtx):Statement =
      val branches = for b <- bi.post yield mkBranchInstance(b,agentCtx)
      MethodSts(
        Variable("use")::
          FunCall("",Nil,Tuple(branches).toString::Nil)::
          Nil
      )

    protected def mkBranchInstance(b:EvidenceMap,agentCtx: AgentOptCtx):FunCall =
      val args =  mkOutArgs(b,agentCtx)
      FunCall(agentCtx.name,Nil,args)


    //protected def mkForkSt(agentCtx: AgentOptCtx):Statement =
    //  agentCtx.fork.get.fork.toList match
    //    case (sp,bi)::Nil =>
    //      //val st  = mkSingleSt(e,in,agentCtx)
    //      //MethodDef(methodName(act),Nil,params,Set(),None,st,None,None)
    //      ???
    //    case l =>
    //      ???
    //



    protected def mkOptionCO(agentCtx: AgentOptCtx):ScalaObject =
      //val initial = FunCall("new "+name,Nil,addForkVar(tVars.map(t=>"true"),"false"))
      val initial = FunCall(agentCtx.name,Nil,agentCtx.initialVals)
      val start = MethodDef("start",Nil,Nil,Set(),None,initial,None)
      val mt    = mkMatchTypes(agentCtx)
      ScalaObject(agentCtx.name,Some(Statements(start::mt)),None)

    protected def mkMatchTypes(agentCtx: AgentOptCtx):List[Statement] =
      val sends:Map[Event,In|Out] = agentCtx.agentCom.sends
      val recs:Map[Event,In|Out]  = agentCtx.agentCom.receives
      val actions = sends++recs
      val eventsByAction  = actions.groupMap(_._2)(_._1)
      for
        (a,es) <- eventsByAction.toList
        if es.size > 1
      yield
        mkMatchType(a,es.toList,agentCtx)

    protected def mkMatchType(act:In|Out,events:List[Event],agentCtx: AgentOptCtx):Statement =
      val cases = for e <- events yield
        mkTypeCase(agentCtx.agentEvid.pre(e),
          mkSingleMatchTypeSt(agentCtx.agentEvid.post(e), agentCtx),
          agentCtx)
      MatchTyp(agentCtx.name++matchTypeName(act),agentCtx.typeVarTypeList,cases)

    protected def mkTypeCase(evid:EvidenceMap,st:TExp,agentCtx: AgentOptCtx):MatchTypCase =
      val pattern =
        for e <- agentCtx.events yield
          if evid.isDefinedAt(e) then
            evid(e)
          else "_"
      MatchTypCase(pattern,st)

    protected def mkSingleMatchTypeSt(evid:EvidenceMap,agentCtx: AgentOptCtx) =
      val outArgs = mkOutTArgs(evid,agentCtx)

      TName(agentCtx.name,Some(outArgs))

    protected def mkOutTArgs(evid:EvidenceMap,agentCtx: AgentOptCtx,prefix:String=""):List[String] =
    //val evid = agentCtx.agentEvid.post(e)
    for (event,tv) <- agentCtx.events.zip(agentCtx.typeVarsList) yield
      if evid.contains(event) then
        evid(event)
      else prefix++tv

    protected def mkForkMatchType(agentCtx: AgentOptCtx):Option[Statement] = agentCtx.fork match
      case Some(info) => info.fork.toList match
        case s::Nil => None
        case l =>
          val cases = mkForkTypeCases(l,agentCtx)
          Some(MatchTyp("Fork",agentCtx.typeVarTypeList,cases))
      case None => None

    protected def mkForkTypeCases(forks:List[(SplitPoint,BranchInfo)],agentCtx: AgentOptCtx):List[MatchTypCase] =
      for (s,bi) <- forks yield
        mkTypeCase(bi.pre,mkSingleMatchTypeStFork(s,bi,agentCtx),agentCtx)

    protected def mkSingleMatchTypeStFork(sp:SplitPoint,bi:BranchInfo,agentCtx: AgentOptCtx):TExp =
      val branches = for b <- bi.post yield mkBranchTInstance(b,agentCtx)
      TTuple(branches)

    protected def mkBranchTInstance(b:EvidenceMap,agentCtx: AgentOptCtx):TExp =
      val args =  mkOutTArgs(b,agentCtx)
      TName(agentCtx.name,Some(args))

  ////// helpers

  protected def mkEvidence(evidences:List[EvidenceMap],agentCtx: AgentOptCtx,prefix:String=""):Set[Evidence] =
    (for  m <- evidences yield
      Evidence(
        for (e,v) <- m yield
          (prefix++agentCtx.typeVariables.event2TVar(e)._1) -> v
      )
      ).toSet

  protected def chName(action:In|Out):String = action match
    case In(a,b,_)  => b.s.toUpperCase++a.s.toUpperCase
    case Out(a,b,_) => a.s.toUpperCase++b.s.toUpperCase



  protected def methodName(a:In|Out):String = a match
    case _:In  => "recv"
    case _:Out => "send"
  protected def paramName(a:In|Out):String = a match
      case _:In  => "from"
      case _:Out => "to"
  protected def roleName(a:Agent):String = "Role"++a.s.toUpperCase
  protected def msgName(m:Msg):String = m.names.capitalize
  protected def matchTypeName(action:In|Out):String = action match
    case In(a,b,m)  => "From"++b.s.toUpperCase++m.names.toUpperCase
    case Out(a,b,m) => "To"++b.s.toUpperCase++m.names.toUpperCase
