//package choreo.api
//
//import choreo.api.LocalAPI.*
//import choreo.api.Code
//import choreo.syntax.Agent
//
///**
// * Created by guillecledou on 15/02/2022
// *
// * Protocol API in Scala
// */
//
//case class LocalAPI(global:SetAPI, locals: List[SingleAPI]) extends Code:
//
//  def toCode(implicit i: Int): String =
//    global.toCode ++ "\n\n" ++ locals.map(l=>l.toCode(i+1)).mkString("\n\n")
//    //locals.map(l=>l.toCode).mkString("\n\n") ++ "\n\n" ++ global.toCode
//
//  def globalNamed(n:String):LocalAPI =
//    this.copy(global = global.named(n))
//
//object LocalAPI:
//
//  def apply(locals:List[SingleAPI]):LocalAPI =
//    LocalAPI(SetAPI(locals),locals)
//
//  class SetAPI(
//    override val name:String,
//    override val typsDef:List[TDef],
//    val extension:Extension,
//    override val methods:List[Method]
//  ) extends ScalaObject(name, typsDef, methods,Nil)
//    with Code:
//    override def toCode(implicit i: Int): String = ind(i) ++
//      s"""object $name:\n\n""" ++
//      sep(typsDef.map(_.toCode(i+1))) ++
//      sep(methods.map(m=>m.toCode(i+1))) ++
//      extension.toCode(i+1)
//
//    def named(n:String):SetAPI =
//     new SetAPI(n,typsDef,extension,methods)
//
//  object SetAPI:
//    //def apply():GlobalAPI = GlobalAPI("",Nil,Extension(),Nil)
//
//    def apply(locals:List[SingleAPI]):SetAPI = mkGlobal(locals)
//
//    def mkGlobal(locals:List[SingleAPI]):SetAPI =
//      new SetAPI("", Nil,mkExtension(locals),mkInit(locals)::Nil)
//
//    def mkInit(locals:List[SingleAPI]):Method =
//      Method(
//        "start",
//        Nil,
//        Set(),
//        MethodCall("",Nil,locals.map(l=>l.apiClass.name++".start()")),
//        None
//      )
//
//    def mkExtension(locals:List[SingleAPI]):Extension =
//      val typVars     = for l <- locals yield l.apiClass.name -> l.apiClass.typVars.map(t=>l.apiClass.name++t)
//      val typVarsList = typVars.flatMap(_._2)
//      val param       = Param("p", TTuple(for (cn,tv) <- typVars yield TName(cn,Some(tv))))
//      val methods     = mkExtMethods(locals)
//      val ended       = mkEnded(typVarsList)
//
//      Extension(typVarsList,param,methods:+ended)
//
//    def mkEnded(typVars:List[String]):ExtMethod =
//      val ev = Evidence(typVars.map(t=>t->"false").toMap)
//      ExtMethod("end",Nil,Set(ev),Variable("p"),None)
//
//    def mkExtMethods(locals:List[SingleAPI]):List[ExtMethod] =
//      val method2Info    =
//        for (i,l) <- (1 to locals.size).zip(locals)
//            m <- l.apiClass.methods
//        yield (m.name,(i,l.apiClass.name,m))
//      val localsByMethod  = method2Info.groupMap(_._1)(_._2)
//      (for (m,l) <- localsByMethod; if m != "end" yield
//        mkExtMethod(m,l.map(p=>p._1->(p._2,p._3)).toMap,locals.size)).toList
//
//    def mkExtMethod(name:String, locals:Map[Int,(String,Method)],args:Int):ExtMethod =
//      val params:List[Param]  = Nil // todo: maybe send(to:A,msg:Msg)... or receive(from:A,msg:Msg) ...
//      val ev:Set[Evidence]    = mkMethodEvidence(name,locals.values)
//      val st:MethodCall       =
//        MethodCall("", Nil, (1 to args).map(i=> if locals.isDefinedAt(i) then s"p._$i.$name()" else s"p._$i.end()").toList)
//
//      ExtMethod(name,params,ev,st,None)
//
//
//    def mkMethodEvidence(name:String, locals:Iterable[(String,Method)]):Set[Evidence] =
//      for (cn,m) <- locals.view.toSet ; e <- m.ev yield e.updKeysPrefix(cn)
//
//  case class SingleAPI(
//    apiClass:LocalAPIClass,
//    apiObj:Option[ScalaObject]
//  ) extends Code:
//    def toCode(implicit i: Int): String = apiObj match
//      case Some(o) => apiClass.toCode ++ "\n\n" ++ o.toCode
//      case None    => apiClass.toCode
//
//    def withClass(c:LocalAPIClass) = this.copy(apiClass=c)
//
//    def withObj(o:ScalaObject) = this.copy(apiObj=Some(o))
//
//  object SingleAPI:
//    def apply():SingleAPI = SingleAPI(LocalAPIClass(),None)
//
//  case class LocalAPIClass(
//    name:String,
//    typVars:List[String],
//    parameters:List[Param],
//    methods:List[Method]
//  ) extends Code:
//    def toCode(implicit i: Int): String = ind(i) ++
//      s"""class $name${brackets(typVars.map(t=>s"$t <: TF"))(i+1)} ${params(parameters.map(_.toString))(i+1)}""" ++ (
//        if methods.nonEmpty then
//          ":\n\n" ++ methods.map(m=>m.toCode(i+1)).mkString("\n\n")
//        else "")
//
//    def named(n:String) = this.copy(name = n)
//    def addTVar(t:String) = this.copy(typVars = typVars:+t)
//    def addParam(p:Param) = this.copy(parameters = parameters:+p)
//    def +(m:Method) = this.copy(methods = methods:+m)
//
//  object LocalAPIClass:
//    def apply():LocalAPIClass = LocalAPIClass("",Nil,Nil,Nil)
//
//  class ScalaObject(
//    val name:String,
//    val typsDef:List[TDef],
//    //extension:Option[Extension],
//    val methods:List[Method],
//    val matchTypes: List[MatchTyp]
//  ) extends Code:
//    def toCode(implicit i: Int): String = ind(i) ++
//      s"""object $name:\n\n""" ++
//      sep(typsDef.map(_.toCode(i+1))) ++
//      sep(matchTypes.map(_.toCode(i+1))) ++
//      methods.map(m=>m.toCode(i+1)).mkString("\n\n")
//
//    def addMatchTypes(mts:List[MatchTyp]):ScalaObject =
//      new ScalaObject(name,typsDef,methods,matchTypes++mts)
//
//  object ScalaObject:
//    def apply():ScalaObject = new ScalaObject("",Nil,Nil,Nil) //ScalaObject("",Nil,None,Nil)
//
//  case class TDef(name:String,typVars:List[String],tExp: TExp) extends Code:
//    def toCode(implicit i: Int): String = ind(i) ++
//      s"""type $name${brackets(typVars)} = ${tExp.toString}"""
//
//  //  Extension
//  case class Extension(typVars:List[String],param:Param,methods:List[ExtMethod]) extends Code:
//    def toCode(implicit i: Int): String = ind(i) ++
//      s"""extension${brackets(typVars.map(t=>s"$t <: TF"))(i+1)}""" ++
//      s"""(${param.toString}) {\n\n""" ++ // todo: upd
//      methods.map(m=>m.toCode(i+1)).mkString("\n\n") ++ ind(i) ++ s"\n${ind(i)}}"
//
//    //def addTVar(t:String) = this.copy(typVars = typVars :+t)
//    //def addTVars(ts:List[String]) = this.copy(typVars = typVars++ts)
//    //
//    //def withParam(p:Param) = this.copy(param = p)
//    //def addParamType(t:TExp) = param.typ match
//    //  case p1@TName(name, typVars) =>
//    //    this.withParam(param.withType(TTuple(p1::t::Nil)))
//    //  case TTuple(typs) =>
//    //    this.withParam(param.withType(TTuple(typs:+t)))
//    //
//    //def +(m:ExtMethod) = this.copy(methods = methods :+ m)
//    //def ++(ms:List[ExtMethod]) = this.copy(methods = methods ++ ms)
//
//  object Extension:
//    def apply():Extension = Extension(Nil, Param("p",TTuple(Nil)),Nil)
//
//  // Parameter
//  case class Param(name:String,typ:TExp) extends Code:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ name ++ ":" ++ typ.toString
//
//    def withType(t:TExp) = this.copy(typ=t)
//  // types
//  trait TExp extends Code:
//    def size():Int = this match
//      case TName(_,_) => 1
//      case TTuple(l) => l.size
//
//  case class TName(name:String,typVars:Option[List[String]]=None) extends TExp:
//    def toCode(implicit i:Int):String = ind(i) ++ (typVars match
//      case Some(vars) => name++"["++vars.mkString(",")++"]"
//      case None => name)
//
//  case class TTuple(typs:List[TExp]) extends TExp:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ typs.mkString("(",",",")")
//
//  // method
//  class Method(
//    val name:String,
//    val parameters:List[Param],
//    val ev:Set[Evidence],
//    val statement: Statement,
//    val returnType:Option[TExp]) extends Code:
//
//    def toCode(implicit i:Int):String =
//      ind(i) ++
//        s"""def $name${params(parameters.map(_.toString))}${mkTExp()} =\n""" ++
//        statement.toCode(i+1)
//
//    protected def mkTExp():String =
//      if returnType.isDefined then
//        ": " ++ returnType.get.toString
//      else ""
//
//  class ExtMethod(
//    override val name:String,
//    override val parameters:List[Param],
//    override val ev:Set[Evidence],
//    override val statement: Statement,
//    override val returnType:Option[TExp]
//  ) extends Method(name,parameters,ev,statement,returnType):
//
//    override def toCode(implicit i:Int):String =
//      ind(i) ++
//        s"""def $name${params(parameters.map(_.toString))}${mkEvidence()(i+1)}${mkTExp()} =\n""" ++
//        statement.toCode(i+1)
//
//    protected def mkEvidence()(implicit i:Int):String =
//      val evStr = ev.map(_.toString).toList
//      val lg    = length(evStr)
//      if ev.isEmpty then ""
//      else if lg <40 then
//        s"""(using ev: ${params(ev.map(_.toString).toList," | ",ln = false)(i+1)})"""
//      else
//        "(\n" ++ ind(i) ++ s"""using ev: ${params(ev.map(_.toString).toList," | ")(i+1)}\n"""++ s"${ind(Integer.max(i-1,0))})"
//      //  "(\n" ++ ind(i) ++ s"""using ev: (\n${ev.map(e=>e.toCode(i+1)).mkString(s" |\n")}\n""" ++
//      //ind(i)++")" ++ s"\n${ind(Integer.max(i-1,0))})"
//
//
//  // Evidence
//  case class Evidence(evidence:Map[String,String]) extends Code:
//    def toCode(implicit i: Int): String =
//      if evidence.nonEmpty then
//        ind(i) ++
//          showEv(evidence.keys.toList) ++
//          "<:<" ++ showEv(evidence.values.toList)
//      else ""
//
//    protected def showEv(args:List[String]):String =
//      if args.size == 1 then args.head
//      else args.mkString("(",",",")")
//
//    def updKeysPrefix(prefix:String):Evidence =
//      Evidence(evidence.map(e=>(prefix++e._1,e._2)))
//  // Statements
//  sealed trait Statement extends Code
//  case class MethodCall(method:String,typVars:List[String],args:List[String]) extends Statement:
//    def toCode(implicit i: Int): String =
//      ind(i) ++
//        s"""$method${brackets(typVars,ln=false)}${params(args,ln=false)}"""
//
//  case class Match(matchVars:List[String],cases:List[Case]) extends Statement:
//    def toCode(implicit i: Int): String =
//      ind(i) ++ s"""(${params(matchVars, ln = false)}: @unchecked) match\n""" ++
//        cases.map(c=>c.toCode(i+1)).mkString("\n")
//
//  case class Variable(name:String) extends Statement:
//    def toCode(implicit i:Int):String = ind(i) ++ name
//
//  // Case
//  case class Case(pattern:List[String],patternTyp:List[String], output:MethodCall) extends Code:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ s"""case ${params(pattern,ln = false)}:${params(patternTyp,ln = false)} => $output"""
//
//  // Match Type
//  case class MatchTyp(name:String, typVars: List[String], cases:List[MatchTypCase]) extends Code:
//    def toCode(implicit i:Int):String =
//      s"""${ind(i)}type $name[
//         |${typVars.map(v=>s"${ind(i+1)}$v <: TF").mkString(",\n")}
//         |${ind(i)}] = ${typVars.mkString("(",",",")")} match
//         |${cases.map(c=>c.toCode(i+1)).mkString("\n")}""".stripMargin
//
//  case class MatchTypCase(pattern:List[String], output:TExp) extends Code:
//    def toCode(implicit i:Int):String =
//      ind(i) ++ s"""case ${params(pattern,ln = false)} => $output"""