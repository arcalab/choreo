package choreo.api

import choreo.npomsets.{Choreo2NPom, NPomset}
import choreo.syntax.{Agent, Choreo}
import choreo.syntax.Choreo.*
import choreo.api.LocalAPI
import choreo.api.LocalAPI.*
import choreo.api.MiniScala.*
import choreo.npomsets.NPomset.*
import choreo.api.Protocol.*

/**
 * Created by guillecledou on 15/02/2022
 *
 * NPomset to Scala APIs
 */
object Protocol//:
  ////todo: end methods, comments, save, handle empty pomsets, object marker
  //def apply(c:Choreo):Set[LocalAPI] =
  //  val npom = Choreo2NPom(c)
  //  val forked = canForked(c)
  //  apply(npom)(forked)
  //
  //def apply(npom:NPomset)(implicit forked:Boolean):Set[LocalAPI] =
  //  val choices = npom.refinements
  //  val pomByAgent = for a <- npom.agents yield a-> choices.map(_.project(a))
  //  for (a,poms) <- pomByAgent yield mkProtocol(a,poms)
  //
  //protected def canForked(c:Choreo):Boolean =
  //  hasParallel(c) && !hasChoiceOrLoop(c)
  //protected def hasChoiceOrLoop(c:Choreo):Boolean = c match
  //  case Seq(c1, c2) => hasChoiceOrLoop(c1) || hasChoiceOrLoop(c2)
  //  case Par(c1, c2) => hasChoiceOrLoop(c1) || hasChoiceOrLoop(c2)
  //  case Choice(c1, c2) => true
  //  case DChoice(c1,c2) => true
  //  case Loop(c) => true
  //  case _ => false
  //
  //protected def hasParallel(c:Choreo): Boolean = c match
  //  case Seq(c1, c2) => hasParallel(c1) || hasParallel(c2)
  //  case Par(c1, c2) => true
  //  case _ => false
  //protected def mkProtocol(a:Agent,poms:List[NPomset])(implicit forked:Boolean):LocalAPI =
  //  val actions = poms.map(p=>p.actions.values.toSet)
  //  val shared  =
  //    if actions.length <= 1 then Set()
  //    else actions.tail.foldRight(actions.head)(_.intersect(_))
  //
  //  val locals  = poms match
  //    case p::Nil =>
  //      val psim = p.simplified
  //      (LocalProtocol(
  //        prot(a.s.toUpperCase++"0"),
  //        psim,
  //        shared.asInstanceOf[Set[In|Out]],
  //        forked
  //      ).api(), psim)::Nil
  //    case _ =>
  //      for
  //        (p,i) <- poms.zipWithIndex
  //        psim = p.simplified
  //      yield
  //        (LocalProtocol(
  //          prot(a.s.toUpperCase+i),
  //          psim,
  //          shared.asInstanceOf[Set[In|Out]]
  //        ).api(), psim)
  //  //mkSetAPI(prot(a.s.toUpperCase), locals)
  //  LocalAPI(SetProtocol(prot(a.s.toUpperCase), locals).setAPI(),locals.map(_._1))//SetProtocol(prot(a.s.toUpperCase), locals).setAPI()
  //
  ////def mkSetAPI(name:String,singles:List[SingleAPI]):LocalAPI =
  ////  val statements = Statements(mkInit(singles)::mkExtension(singles)::Nil)
  ////  val set = ScalaObject(name,statements,None)
  ////  LocalAPI(SetAPI(set),singles)
  //////new SetAPI("", Nil,mkExtension(locals),mkInit(locals)::Nil)
  //
  //sealed trait SplitingPoint
  //case class SplitingEvent(e:Event) extends SplitingPoint
  //case class SplitingInit(events:Set[Event]) extends SplitingPoint
  //
  //class SetProtocol(name:String,singlesApi:List[(SingleAPI,NPomset)]):
  //
  ////protected lazy val :List[Event] = pom.events.toSet.toList.sorted
  ////protected lazy val tVars:List[String] = events.map(tVar)
  ////protected lazy val classParams:List[String] = events.map(tParam)
  //  protected lazy val singles = singlesApi.map(_._1)
  //  protected lazy val forkClasName = singles.head.clas.name
  //  protected lazy val forkClas = singles.head.clas
  //  protected lazy val forkPom = singlesApi.head._2
  //
  //  def setAPI():SetAPI =
  //    var statements:List[Statement] =List(mkInit(singles))
  //    var extension = mkExtension(singles)
  //    if singles.size == 1 then
  //      val (fork,mt) = mkForks()
  //      extension = extension.addMethod(fork)
  //      if mt.isDefined then statements = statements:+mt.get
  //    val set = ScalaObject(name,Statements(statements:+extension),None)
  //    SetAPI(set)
  //
  //  // todo: for now it assumes no choices (singleApis is a singleton)
  //  protected def mkForks():(MethodDef,Option[MatchTyp]) =
  //    val (singleApi,pom) = singlesApi.head
  //    val branchingPoints = findBranchingPoints(pom)
  //    println(s"branchingPoints = ${branchingPoints}")
  //    val forkEvidence = mkForkEvidence(branchingPoints,pom)
  //    println(s"forkevidence = ${forkEvidence}")
  //    mkForkMethod(forkEvidence,singleApi,pom)
  //    //val forks = for e <- branchingEvents yield mkFork(pom,e)
  //    //if branchingInit.size>1 then
  //    //  setAPI.addMethods(forks+mkFork(pom,branchingInit))
  //    //else setAPI.addMethods(forks)

  //  protected def mkForkMethod(
  //    ev:Map[SplitingPoint,Evidence],
  //    singleApi:SingleAPI,
  //    pom:NPomset
  //  ): (MethodDef,Option[MatchTyp]) =
  //    val (st,mt)   = mkForkSt(ev)
  //    val typ       = Some(mkForkReturn(mt,ev))
  //
  //    (MethodDef("fork",Nil,Nil,ev.values.toSet,typ,st,None),mt)
  //
  //  protected def mkForkReturn(mt:Option[MatchTyp],ev:Map[SplitingPoint,Evidence]):TExp = (mt) match
  //    case Some(m) => TName(m.name,Some(m.typVars))
  //    case None    =>
  //      val (e,_) = ev.head
  //      mkBranchType(e)
  //
  //  protected def mkForkSt(ev:Map[SplitingPoint,Evidence])
  //    :(Statement,Option[MatchTyp]) = ev.toList match
  //      case List((e,ev)) =>
  //        val branches =  mkBranchObjects(e)
  //        println(s"branches = ${branches}")
  //        (FunCall("",Nil,branches.map(_.toString)),None)
  //      case _ =>
  //        val cases = mkForkCases(ev)
  //        val tcases = mkForkMatchTypeCases(ev)
  //        (Match(forkClas.parameters.map(_.name),cases),
  //          Some(MatchTyp("Fork",forkClas.typeVariables.map(t=>forkClasName+t),tcases)))
  //
  //  def mkForkMatchTypeCases(ev:Map[SplitingPoint,Evidence]):List[MatchTypCase] =
  //    for (e,evid) <- ev.toList yield mkForkMatchTypeCase(e,evid)
  //
  //  def mkForkMatchTypeCase(point:SplitingPoint, ev:Evidence):MatchTypCase =
  //    //val pattern     = forkClas.typeVariables.map(t=> ev.evidence.getOrElse(t,"_"))
  //    val patternTExp = forkClas.parameters.map(p=>ev.evidence.getOrElse(forkClasName++p.typ.toString,"_"))
  //    //val patternTExp2 = patternTExp1.map(p => if p._2 then p._1 else "_")
  //    val output      = mkBranchType(point)
  //    MatchTypCase(patternTExp,output)
  //
  //  def mkBranchType(p:SplitingPoint):TExp = p match
  //    case SplitingEvent(e) =>
  //      val succ = forkPom.succ.getOrElse(e,Set())
  //      val res = for
  //        s <- succ.toList
  //        others = succ-s
  //        toFalse =  others++others.flatMap(e=>forkPom.allSuccesors(e))
  //      yield
  //        mkBranchType(s,toFalse)
  //      TTuple(res.toList)
  //    case SplitingInit(es) =>
  //      val res = for
  //        e <- es.toList
  //        others = es-e
  //        toFalse =  others++others.flatMap(e=>forkPom.allSuccesors(e))
  //      yield
  //        mkBranchType(e,toFalse)
  //      TTuple(res)
  //
  //  def mkBranchType(e:Event,toFalse:Set[Event]):TName=
  //    val toFalseName = toFalse.map(tVar)
  //    val args = for p <- forkClas.typeVariables yield
  //      if toFalseName.contains(p) then "false"
  //      else forkClasName+p
  //    TName(forkClasName,Some(args))
  //
  //  def mkForkCases(ev:Map[SplitingPoint,Evidence]):List[Case] =
  //    for (e,evid) <- ev.toList yield mkForkCase(e,evid)
  //
  //  protected def mkForkCase(p:SplitingPoint,ev:Evidence):Case =
  //    //val pattern     = forkClas.parameters.map(e=>"_")
  //    val pattern = forkClas.parameters.map(p=>ev.evidence.getOrElse(forkClasName++p.typ.toString,"_"))
  //    //val patternTExp2 = patternTExp1.map(p => if p._2 == "_" then p._1 else "_")
  //    val output      = FunCall("",Nil,mkBranchObjects(p).map(_.toString))//,pom.succ.getOrElse(e,Set()).toList))
  //    Case(pattern,pattern,output)
  //
  //  def mkBranchObjects(p:SplitingPoint):List[FunCall] = p match
  //    case SplitingEvent(e) => mkBranchObjects(e)
  //    case SplitingInit(es) => mkBranchObjects(es.toList)
  //
  //  def mkBranchObjects(es:List[Event]):List[FunCall] =
  //    val esSet = es.toSet
  //    for
  //      e <- es
  //      others = esSet - e
  //      toFalse = others++others.flatMap(e=>forkPom.allSuccesors(e))
  //    yield
  //      mkBranchObject(e,toFalse)
  //
  //  def mkBranchObjects(e:Event):List[FunCall] =
  //    val succ = forkPom.succ.getOrElse(e,Set())
  //    for
  //      s <- succ.toList
  //      others = succ-s
  //      toFalse =others++others.flatMap(e=>forkPom.allSuccesors(e))
  //    yield
  //      mkBranchObject(s,toFalse)
  //
  //  def mkBranchObject(e:Event,toFalse:Set[Event]):FunCall =
  //    val toFalseName = toFalse.map(tParam)
  //    val args = for p <- forkClas.parameters yield
  //      if toFalseName.contains(p.name) then "false"
  //      else "p"++"."++p.name
  //
  //    FunCall(forkClasName,Nil,args)
  //
  //
  //
  //  protected def mkForkEvidence(branchingPoints:Set[SplitingPoint],pom:NPomset):Map[SplitingPoint,Evidence] =
  //    (for bp <- branchingPoints yield bp->mkForkEvidence(bp,pom)).toMap
  //
  //  protected def mkForkEvidence(branchingPoint:SplitingPoint,pom:NPomset):Evidence =
  //    //val (singleApi,pom) = singlesApi.head
  //    branchingPoint match
  //      case SplitingEvent(e) =>
  //        val succ = pom.succ.getOrElse(e,Set()).map(tVar)
  //        Evidence(succ.map(s=>(forkClasName++s)->"true").toMap+((forkClasName++tVar(e))->"false"))
  //      case SplitingInit(es) =>
  //        Evidence(es.map(e=>(forkClasName+tVar(e))->"true").toMap)
  //
  //
  //  protected def findBranchingPoints(pom:NPomset):Set[SplitingPoint] =
  //    val branchingEvents:Set[SplitingPoint] = findBranchingEvents(pom).map(e=>SplitingEvent(e))
  //    val branchingInit = pom.minimum()
  //    if branchingInit.size>1 then
  //      branchingEvents + SplitingInit(branchingInit)
  //    else
  //      branchingEvents
  //
  //  protected def findBranchingEvents(pom:NPomset):Set[Event] =
  //    for
  //      e <- pom.events.toSet
  //      directSucc = pom.succ.getOrElse(e,Set())
  //      if directSucc.size>1
  //    yield
  //      e
  //
  //  protected def findMergingEvents(pom:NPomset):Set[Event] =
  //    for
  //      e <- pom.events.toSet
  //      directPred = pom.realPred(e)
  //      if directPred.size>1
  //    yield
  //      e
  //
  //
  //
  //
  //  def mkInit(locals:List[SingleAPI]):MethodDef =
  //    val init = FunCall("",Nil,locals.map(l=>l.clas.name++".start()"))
  //    MethodDef("start",Nil,Nil,Set(),None,init,None)
  //
  //  def mkExtension(locals:List[SingleAPI]):Extension =
  //    val typVars     =
  //      for
  //        l <- locals
  //        tvars = l.clas.typeVariables
  //        tvarsT = l.clas.typeVariablesTExp
  //        zip = tvars.zip(tvarsT)
  //      yield l.clas.name -> zip.map(t=>(l.clas.name++t._1,t._2))
  //    val typVarsList = typVars.flatMap(_._2).map(_._1)
  //    val typVarsTExp = typVars.flatMap(_._2).map(_._2)
  //    val param       = Param("p", TTuple(for (cn,tv) <- typVars yield TName(cn,Some(tv.map(_._1)))))
  //    val methods     = mkExtMethods(locals)
  //    val ended       = mkEnded(typVarsList)
  //
  //    Extension(typVarsList,typVarsTExp,param,methods:+ended)
  //
  //  def mkEnded(typVars:List[String]):MethodDef =
  //    val ev = Evidence(typVars.map(t=>t->"false").toMap)
  //    MethodDef("end",Nil,Nil,Set(ev),None,Variable("p"),None)
  //
  //  def mkExtMethods(locals:List[SingleAPI]):List[MethodDef] =
  //    val method2Info    =
  //      for (i,l) <- (1 to locals.size).zip(locals)
  //          m <- getMethods(l.clas.statements)
  //      yield (m.name,(i,l.clas.name,m))
  //    val localsByMethod  = method2Info.groupMap(_._1)(_._2)
  //    (for (m,l) <- localsByMethod; if m != "end" yield
  //      mkExtMethod(m,l.map(p=>p._1->(p._2,p._3)).toMap,locals.size)).toList
  //
  //  def mkExtMethod(name:String, locals:Map[Int,(String,MethodDef)],numArgs:Int):MethodDef =
  //    val params:List[Param]  = Nil // todo: maybe send(to:A,msg:Msg)... or receive(from:A,msg:Msg) ...
  //    val ev:Set[Evidence]    = mkMethodEvidence(name,locals.values)
  //    val args = if numArgs == 1 then
  //      s"p.$name()"::Nil
  //    else
  //      (1 to numArgs).map(i=> if locals.isDefinedAt(i) then s"p._$i.$name()" else s"p._$i.end()").toList
  //    val st = FunCall("", Nil, args)
  //
  //    MethodDef(name,Nil, params,ev,None,st,None)
  //
  //  def getMethods(statement:Statement):List[MethodDef] = statement match
  //    case m:MethodDef => List(m)
  //    case Statements(l) => l.flatMap(s=>getMethods(s))
  //    case _ => Nil
  //
  //
  //  def mkMethodEvidence(name:String, locals:Iterable[(String,MethodDef)]):Set[Evidence] =
  //    for (cn,m) <- locals.view.toSet ; e <- m.ev yield e.updKeysPrefix(cn)
  //
  //
  //protected def prot(name:String):String = name
  //protected def tVar(i:Int):String = "V"+i
  //protected def tParam(i:Int):String = "v"+i
  //protected def mType(clas:String,ch:In|Out):String = "`"++clas++friendly(ch)++"Type`"
  //protected def friendly(ch:In|Out):String = ch match
  //  case In(a,b,msg) => s"$a$b?${msg.names}"
  //  case Out(a,b,msg) => s"$a$b!${msg.names}"
  //protected def method(c:In | Out):String = c match
  //  case Out(a,b,msg) => s"""to_${b.s}_${msg.names}"""
  //  case In(a,b,msg) => s"""from_${b.s}_${msg.names}"""
  //
  //class LocalProtocol(name:String,pom:NPomset,shared:Set[In|Out],forked:Boolean=false):
  //
  //  protected lazy val events:List[Event] = pom.events.toSet.toList.sorted
  //  protected lazy val tVars:List[String] = events.map(tVar)
  //  protected lazy val classParams:List[String] = events.map(tParam)
  //
  //  def api():SingleAPI =
  //    val (clas,mt) = mkClass()
  //    val co = mkCompanionObj(clas,mt)
  //    SingleAPI(clas,co) // ScalaObject(name, clas,Some(mkObj(mt))))
  //
  //  protected def mkCompanionObj(clas:ScalaClass,mts:List[MatchTyp]):ScalaObject =
  //    val nInstance = FunCall("new "+name,Nil,addForkVar(tVars.map(t=>"true"),"false"))
  //    val init = MethodDef("start",Nil,Nil,Set(),None,nInstance,None)
  //    new ScalaObject(name,Statements(init+:mts),None)
  //
  //  protected def mkClass():(ScalaClass,List[MatchTyp]) =
  //    var params  = for e <- events yield Param(tParam(e),TName(tVar(e),None))
  //    if forked then params :+= Param("f",TName("F",None))
  //    val methods = mkMethods()
  //    val endMethod = mkLocalEnd()
  //    (
  //      ScalaClass(name,
  //        addForkVar(tVars,"F"),
  //        addForkVar(tVars.map(_=>"TF"),"FS"),
  //        params,
  //        Statements(methods.map(_._1):+endMethod),None
  //      ),
  //      methods.flatMap(_._2)
  //    )
  //
  //  protected def mkLocalEnd():MethodDef =
  //    val tVarsVal    = tVars.map(_=>"false")
  //    val finalClass  = FunCall(s"new $name",Nil,addForkVar(tVarsVal,"f"))
  //    val typ         = TName(name,Some(addForkVar(tVarsVal,"F")))
  //    MethodDef("end",Nil,Nil,Set(),Some(typ),finalClass,None)
  //
  //  protected def mkMethods():List[(MethodDef,Option[MatchTyp])] =
  //    val eventsByLabel = pom.actions.groupMap(_._2)(_._1).toList
  //    for (lbl,es) <- eventsByLabel yield
  //      mkMethod(lbl.asInstanceOf[In|Out],es.toList)
  //
  //  protected def mkMethod(ch:In|Out,methodEv:List[Event]):(MethodDef,Option[MatchTyp]) =
  //    val params    = Nil // todo if send(from:A,msg:Msg) ...
  //    val ev        = mkEvidence(methodEv)
  //    val (st,mt)   = mkSt(ch,ev)
  //    val typ       = Some(mkReturn(mt,ev))
  //    (MethodDef(method(ch),Nil,params,ev.map(_._2).toSet,typ,st,None),mt)
  //
  //  protected def mkReturn(mt:Option[MatchTyp],ev:List[(Event,Evidence)]):TExp = (mt) match
  //    case Some(m) => TName(m.name,Some(tVars))
  //    case None    =>
  //      val (e,_) = ev.head
  //      TName(name,Some(mkTVars(e)))
  //
  //  protected def mkEvidence(events:List[Event]):List[(Event,Evidence)] =
  //    for e <- events yield e->mkEvidence(e)
  //
  //  protected def mkEvidence(e:Event):Evidence =
  //    val pre = pom.realPred(e)
  //    Evidence(pre.map(e1=>tVar(e1)->"false").toMap+(tVar(e)->"true"))
  //
  //  protected def mkSt(ch:In|Out,ev:List[(Event,Evidence)]):(Statement,Option[MatchTyp]) =
  //    if shared.contains(ch) then
  //      mkStWithEnd(ch,ev)
  //    else ev match
  //      case List((e,_)) =>
  //        //val suc = pom.succ.getOrElse(e,Set()).toList
  //        val args = mkArgs(e)//,suc)
  //        (FunCall(name,Nil,args),None)
  //      case _ =>
  //        val cases = mkCases(ev)
  //        val tcases = mkMatchCases(ev)
  //        (Match(addForkVar(classParams,"f"),cases),
  //          Some(MatchTyp(mType(name,ch),addForkVar(tVars,"F"),tcases)))
  //
  //  protected def mkStWithEnd(ch:In|Out,ev:List[(Event,Evidence)]):(Statement,Option[MatchTyp]) =
  //    val cases  = mkCases(ev):+mkEndCase()//(ch,ev)
  //    val tcases = mkMatchCases(ev):+mkMatchTypEndCase()
  //    (Match(addForkVar(classParams,"f"),cases),
  //      Some(MatchTyp(mType(name,ch),addForkVar(tVars,"F"),tcases)))
  //
  //  protected def mkEndCase():Case =
  //    val pattern     = addForkVar(events.map(e=>"_"),"_")
  //    val patternTExp = if events.length == 1 then "false"::Nil else pattern
  //    val output      = FunCall("end",Nil,Nil)
  //    Case(pattern,patternTExp,output)
  //
  //  protected def mkMatchTypEndCase():MatchTypCase =
  //    val pattern = addForkVar(if events.length == 1 then "false"::Nil else events.map(e=>"_"),"_")
  //    val output  = TName(name,Some(addForkVar(tVars.map(v=>"false"),"F")))
  //    MatchTypCase(pattern,output)
  //
  //  //protected def mkEndCase(ch:In|Out,ev:List[(Event,Evidence)]):Case =
  //  //  val evVars      = ev.flatMap(e=>e._2.evidence.collect({case (k,v) if v=="true" => k->"false"})).toMap
  //  //  val pattern     = events.map(e=>"_")
  //  //  val patternTExp = tVars.map(t=> evVars.getOrElse(t,"_"))
  //  //  val output      = MethodCall("end",Nil,Nil)
  //  //  Case(pattern,patternTExp,output)
  //
  //  protected def mkArgs(e:Event):List[String] = // ,suc:List[Event]):List[String] =
  //    val res = for e1 <- events yield
  //      if e1 == e then "false"
  //      //else if suc.contains(e1) then "true"
  //      else tParam(e1)
  //    addForkVar(res,e.toString)
  //
  //  protected def mkTVars(e:Event):List[String] = // ,suc:List[Event]):List[String] =
  //    val res = for e1 <- events yield
  //      if e1 == e then "false"
  //      //else if suc.contains(e1) then "true"
  //      else tVar(e1)
  //    addForkVar(res,e.toString)
  //
  //  protected def mkCases(ev:List[(Event,Evidence)]):List[Case] =
  //    for (e,evid) <- ev yield mkCase(e,evid)
  //
  //  protected def mkCase(e:Event,ev:Evidence):Case =
  //    val pattern     = addForkVar(events.map(e=>"_"),"_")
  //    val patternTExp = addForkVar(tVars.map(t=> ev.evidence.getOrElse(t,"_")),"_")
  //    val output      = FunCall(name,Nil,mkArgs(e))//,pom.succ.getOrElse(e,Set()).toList))
  //    Case(pattern,patternTExp,output)
  //
  //  protected def mkMatchCases(ev:List[(Event,Evidence)]):List[MatchTypCase] =
  //    for (e,evid) <- ev yield mkMatchCase(e,evid)
  //
  //  protected def mkMatchCase(e:Event,ev:Evidence):MatchTypCase =
  //    val pattern     = addForkVar(tVars,"F").map(t=> ev.evidence.getOrElse(t,"_"))
  //    val output      = TName(name,Some(mkTVars(e)))//,pom.succ.getOrElse(e,Set()).toList))
  //    MatchTypCase(pattern,output)
  //
  //  protected def addForkVar(list:List[String], value:String):List[String] =
  //    if forked then list :+ value else list