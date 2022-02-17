package choreo.api

import choreo.npomsets.NPomset
import choreo.syntax.Agent
import choreo.syntax.Choreo.*
import choreo.api.ScalaProtocol
import choreo.api.ScalaProtocol.*
import choreo.npomsets.NPomset.*

/**
 * Created by guillecledou on 15/02/2022
 *
 * NPomset to Scala APIs
 */
object Protocol:
  //todo: end methods, comments, save, handle empty pomsets, object marker

  def apply(npom:NPomset):Set[ScalaProtocol] =
    val choices = npom.refinements
    val pomByAgent = for a <- npom.agents yield a-> choices.map(_.project(a))
    for (a,poms) <- pomByAgent yield mkProtocol(a,poms)

  protected def mkProtocol(a:Agent,poms:List[NPomset]):ScalaProtocol =
    val locals =
      for (p,i) <- poms.zipWithIndex yield
        LocalProtocol(prot(a.s.toUpperCase+i),p.simplified).api()
    val sp = ScalaProtocol(locals)
    sp.globalNamed(prot(a.s.toUpperCase))

  protected def prot(name:String):String = name
  protected def tVar(i:Int):String = "V"+i
  protected def tParam(i:Int):String = "v"+i
  protected def mType(clas:String,ch:In|Out):String = "`"++clas++friendly(ch)++"Type`"
  protected def friendly(ch:In|Out):String = ch match
    case In(a,b,msg) => s"$a$b?${msg.names}"
    case Out(a,b,msg) => s"$a$b!${msg.names}"
  protected def method(c:In | Out):String = c match
    case Out(a,b,msg) => s"""to_${b.s}_${msg.names}"""
    case In(a,b,msg) => s"""from_${a.s}_${msg.names}"""

  class LocalProtocol(name:String,pom:NPomset):

    protected lazy val events:List[Event] = pom.events.toSet.toList.sorted
    protected lazy val tVars:List[String] = events.map(tVar)
    protected lazy val classParams:List[String] = events.map(tParam)

    def api():LocalAPI =
      val (clas,mt) = mkClass()
      LocalAPI(clas,Some(mkObj(mt)))

    protected def mkClass():(LocalAPIClass,List[MatchTyp]) =
      val params  = for e <- events yield Param(tParam(e),TName(tVar(e),None))
      val methods = mkMethods()
      (LocalAPIClass(name,tVars,params,methods.map(_._1)),methods.map(_._2).flatten)

    protected def mkObj(mts:List[MatchTyp]):ScalaObject =
      val nInstance = MethodCall("new "+name,Nil,tVars.map(t=>"true"))
      val init = Method("start",Nil,Set(),nInstance,None)
      new ScalaObject(name,Nil,init::Nil, mts)

    protected def mkMethods():List[(Method,Option[MatchTyp])] =
      val eventsByLabel = pom.actions.groupMap(_._2)(_._1).toList
      for (lbl,es) <- eventsByLabel yield
        mkMethod(lbl.asInstanceOf[In|Out],es.toList)

    protected def mkMethod(ch:In|Out,methodEv:List[Event]):(Method,Option[MatchTyp]) =
      val params    = Nil // todo if send(from:A,msg:Msg) ...
      val ev        = mkEvidence(methodEv)
      val (st,mt)   = mkSt(ch,ev)
      val typ       = Some(mkReturn(ch,ev))
      (Method(method(ch),params,ev.map(_._2).toSet,st,typ),mt)

    protected def mkReturn(ch:In|Out,ev:List[(Event,Evidence)]):TExp = ev match
      case List((e,evid)) => TName(name,Some(mkArgs(e,pom.succ.getOrElse(e,Set()).toList)))
      case _ => TName(mType(name,ch),Some(tVars))

    protected def mkEvidence(events:List[Event]):List[(Event,Evidence)] =
      for e <- events yield e->mkEvidence(e)

    protected def mkEvidence(e:Event):Evidence =
      val pre = pom.realPred(e)
      Evidence(pre.map(e1=>tVar(e1)->"false").toMap+(tVar(e)->"true"))

    protected def mkSt(ch:In|Out,ev:List[(Event,Evidence)]):(Statement,Option[MatchTyp]) = ev match
      case List((e,_)) =>
        val suc = pom.succ.getOrElse(e,Set()).toList // todo check it is the direct succ
        val args = mkArgs(e,suc)
        (MethodCall(name,Nil,args),None)
      case _ =>
        val cases = mkCases(ev)
        (Match(classParams,cases),Some(MatchTyp(mType(name,ch),tVars,cases)))

    protected def mkArgs(e:Event,suc:List[Event]):List[String] =
      for e1 <- events yield
        if e1 == e then "false"
        else if suc.contains(e1) then "true"
        else tParam(e1)

    protected def mkCases(ev:List[(Event,Evidence)]):List[Case] =
      for (e,evid) <- ev yield mkCase(e,evid)

    protected def mkCase(e:Event,ev:Evidence):Case =
      val pattern     = events.map(e=>"_")
      val patternTExp = tVars.map(t=> ev.evidence.getOrElse(t,"_"))
      val output      = MethodCall(name,Nil,mkArgs(e,pom.succ.getOrElse(e,Set()).toList))
      Case(pattern,patternTExp,output)
