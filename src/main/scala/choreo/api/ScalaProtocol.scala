package choreo.api

import choreo.api.ScalaProtocol.*
import choreo.api.Code
import choreo.syntax.Agent

/**
 * Created by guillecledou on 15/02/2022
 *
 * Protocol API in Scala
 */

case class ScalaProtocol(
  global:GlobalAPI,
  locals: List[LocalAPI]
) extends Code:
  def toCode(implicit i: Int): String =
    locals.map(l=>l.toCode).mkString("\n\n") ++ "\n\n" ++ global.toCode

  def globalNamed(n:String):ScalaProtocol =
    this.copy(global = global.named(n))
  //def fromLocals(ls:List[LocalAPI]) = ScalaProtocol(global.fromLocals(ls),ls)

  //def addLocal(l:LocalAPI) =
  //  //val ng = global.extension
  //  this.copy(locals = locals:+l)
  //
  //def withGlobal(g:GlobalAPI) = this.copy(global=g)


object ScalaProtocol:

  //def apply():ScalaProtocol = ScalaProtocol(GlobalAPI(),Nil)
  def apply(locals:List[LocalAPI]):ScalaProtocol = ScalaProtocol(GlobalAPI(locals),locals)


  class GlobalAPI(
    override val name:String,
    override val typsDef:List[TDef],
    val extension:Extension,
    override val methods:List[Method]
  ) extends ScalaObject(name, typsDef, methods,Nil)
    with Code:
    override def toCode(implicit i: Int): String = ind(i) ++
      s"""object $name:\n""" ++
      typsDef.map(_.toCode(i+1)).mkString("\n") ++
      methods.map(m=>m.toCode(i+1)).mkString("\n\n") ++ "\n\n" ++
      extension.toCode(i+1)+"\n"


    def named(n:String):GlobalAPI =
     new GlobalAPI(n,typsDef,extension,methods)
    //def updFrom(l:LocalAPI):GlobalAPI =
    //  val ext = updExtensionParams(extension,l)
    //  val met = updExtMethods(l)
    //  this.copy(extension = ext,methods = met)
    //
    //protected def updExtMethods(l:LocalAPI):Map[String,ExtMethod] =
    //  val localNames  = l.localClass.methods.keys.toSet
    //  val globalNames = methods.keys.toSet
    //  val newNames    = localNames.diff(globalNames)
    //  val nm1         = for (n,m) <- extension.methods yield (n,m.updStatement(localNames))
    //  val nm2         = mkGlobalMethods(
    //    l.localClass.methods.filter(m=>newNames.contains(m._1)),l.localClass.name)
    //  nm1++nm2
    //
    //protected def mkGlobalMethods(methods: Map[String,Method],className:String):Map[String,ExtMethod] =
    //  for (n,m) <- methods yield n->mkGlobalMethod(n,m,className)
    //
    //protected def mkGlobalMethod(name: String, method: Method,className:String):ExtMethod =
    //  ExtMethod(name,
    //    Nil,
    //    method.ev.map(e=>e.updKeysPrefix(className)),
    //    MethodCall(name,Nil,s"p._2.$name"::Nil),
    //    None
    //  )

  object GlobalAPI:
    //def apply():GlobalAPI = GlobalAPI("",Nil,Extension(),Nil)

    def apply(locals:List[LocalAPI]):GlobalAPI = mkGlobal(locals)

    def mkGlobal(locals:List[LocalAPI]):GlobalAPI =
      new GlobalAPI("", Nil,mkExtension(locals),mkInit(locals)::Nil)

    def mkInit(locals:List[LocalAPI]):Method =
      Method(
        "start",
        Nil,
        Set(),
        MethodCall("",Nil,locals.map(l=>l.apiClass.name++".start")),
        None
      )

    def mkExtension(locals:List[LocalAPI]):Extension =
      val typVars = for l <- locals yield l.apiClass.name -> l.apiClass.typVars.map(t=>l.apiClass.name++t)
      val param   = Param("p", TTuple(for (cn,tv) <- typVars yield TName(cn,Some(tv))))
      val methods = mkExtMethods(locals)

      Extension(typVars.flatMap(_._2),param,methods)

    def mkExtMethods(locals:List[LocalAPI]):List[ExtMethod] =
      val method2Info    =
        for (i,l) <- (1 to locals.size).zip(locals)
            m <- l.apiClass.methods
        yield (m.name,(i,l.apiClass.name,m))
      val localsByMethod  = method2Info.groupMap(_._1)(_._2)
      (for (m,l) <- localsByMethod yield
        mkExtMethod(m,l.map(p=>p._1->(p._2,p._3)).toMap,locals.size)).toList

    def mkExtMethod(name:String, locals:Map[Int,(String,Method)],args:Int):ExtMethod =
      val params:List[Param]  = Nil // todo: maybe send(to:A,msg:Msg)... or receive(from:A,msg:Msg) ...
      val ev:Set[Evidence]    = mkMethodEvidence(name,locals.values)
      val st:MethodCall       =
        MethodCall("", Nil, (1 to args).map(i=> if locals.isDefinedAt(i) then s"p._$i.$name" else s"p._$i.end").toList)

      ExtMethod(name,params,ev,st,None)

    //def mkGlobalBegin(locals:List[ScalaLocalAPI]):Method =
    //  val block = for l <- locals yield
    //  Method("begin",List(),Set(),TypTuple(),None)

    def mkMethodEvidence(name:String, locals:Iterable[(String,Method)]):Set[Evidence] =
      for (cn,m) <- locals.view.toSet ; e <- m.ev yield e.updKeysPrefix(cn)


    //protected def updExtensionParams(ext:Option[Extension],l:LocalAPI):Option[Extension] =
    //  val extTVars   = l.localClass.typVars.map(v=>l.localClass.name+v)
    //  val paramType  = TName(l.localClass.name,Some(extTVars))
    //  val res = ext match
    //    case Some(e) => e.addTVars(extTVars)
    //      .addParamType(paramType)
    //    case None => Extension(
    //      extTVars,
    //      Param("p",paramType),
    //      Map()
    //    )
    //  Some(res)

  case class LocalAPI(
    apiClass:LocalAPIClass,
    apiObj:Option[ScalaObject]
  ) extends Code:
    def toCode(implicit i: Int): String = apiObj match
      case Some(o) => apiClass.toCode ++ "\n\n" ++ o.toCode
      case None    => apiClass.toCode

    def withClass(c:LocalAPIClass) = this.copy(apiClass=c)

    def withObj(o:ScalaObject) = this.copy(apiObj=Some(o))

  object LocalAPI:
    def apply():LocalAPI = LocalAPI(LocalAPIClass(),None)

  case class LocalAPIClass(
    name:String,
    typVars:List[String],
    parameters:List[Param],
    methods:List[Method]
  ) extends Code:
    def toCode(implicit i: Int): String = ind(i) ++
      s"""case class $name${brackets(typVars)(i+1)} ${params(parameters.map(_.toString))}:\n""" ++
      methods.map(m=>m.toCode(i+1)).mkString("\n\n")

    def named(n:String) = this.copy(name = n)
    def addTVar(t:String) = this.copy(typVars = typVars:+t)
    def addParam(p:Param) = this.copy(parameters = parameters:+p)
    def +(m:Method) = this.copy(methods = methods:+m)

    def contains(method:String):Boolean = methods.find(_.name==method).isDefined

  object LocalAPIClass:
    def apply():LocalAPIClass = LocalAPIClass("",Nil,Nil,Nil)

  class ScalaObject(
    val name:String,
    val typsDef:List[TDef],
    //extension:Option[Extension],
    val methods:List[Method],
    val matchTypes: List[MatchTyp]
  ) extends Code:
    def toCode(implicit i: Int): String = ind(i) ++
      s"""object $name:""" ++
      typsDef.map(_.toCode(i+1)).mkString("\n") ++ "\n" ++
      //(if extension.isDefined then extension.get.toCode(i+1)+"\n" else "") ++
      methods.map(m=>m.toCode(i+1)).mkString("\n\n")

    def addMatchTypes(mts:List[MatchTyp]):ScalaObject =
      new ScalaObject(name,typsDef,methods,matchTypes++mts)

  object ScalaObject:
    def apply():ScalaObject = new ScalaObject("",Nil,Nil,Nil) //ScalaObject("",Nil,None,Nil)

  case class TDef(name:String,typVars:List[String],tExp: TExp) extends Code:
    def toCode(implicit i: Int): String = ind(i) ++
      s"""type $name${brackets(typVars)} = ${tExp.toString}"""

  //  Extension
  case class Extension(typVars:List[String],param:Param,methods:List[ExtMethod]) extends Code:
    def toCode(implicit i: Int): String = ind(i) ++
      s"""extension${brackets(typVars)(i+1)}""" ++
      s"""(${param.toString}) {\n""" ++ // todo: upd
      methods.map(m=>m.toCode(i+1)).mkString("\n\n") ++ ind(i) ++ s"\n${ind(i)}}"

    def addTVar(t:String) = this.copy(typVars = typVars :+t)
    def addTVars(ts:List[String]) = this.copy(typVars = typVars++ts)

    def withParam(p:Param) = this.copy(param = p)
    def addParamType(t:TExp) = param.typ match
      case p1@TName(name, typVars) =>
        this.withParam(param.withType(TTuple(p1::t::Nil)))
      case TTuple(typs) =>
        this.withParam(param.withType(TTuple(typs:+t)))

    def +(m:ExtMethod) = this.copy(methods = methods :+ m)
    def ++(ms:List[ExtMethod]) = this.copy(methods = methods ++ ms)

  object Extension:
    def apply():Extension = Extension(Nil, Param("p",TTuple(Nil)),Nil)
    //def updFrom(l:LocalAPI):GlobalAPI =
    //  val met = updExtMethods(l)
    //  val ext = updExtensionParams(l)
    //  this.copy(extension = ext,methods = met)
    //
    //protected def updFromLocalMethod(className:String,method:Method):Map[String,ExtMethod] =
    //  if methods.contains(method.name) then
    //    for (n,m) <- methods yield
    //      if n == method.name then
    //        m.updFromLocalMethod(className,m)
    //    methods.updated(
    //      method.name,
    //      methods(method.name).
    //    )
    //  else
    //    methods + (method.name->mkExtMethod(className,method))
    //
    //protected def mkExtMethod(className:String,method:Method):ExtMethod =
    //  ExtMethod(method.name,
    //    Nil,
    //    method.ev.map(e=>e.updKeysPrefix(className)),
    //    MethodCall(className,method,param.typ.size()),
    //    None
    //  )
    //
    //protected def updExtensionParams(l:LocalAPI):Extension =
    //  val extTVars   = l.apiClass.typVars.map(v=>l.apiClass.name+v)
    //  val paramType  = TName(l.apiClass.name,Some(extTVars))
    //  this.addTVars(extTVars)
    //    .addParamType(paramType)
    //  //val res = ext match
    //  //  case Some(e) => e.addTVars(extTVars)
    //  //    .addParamType(paramType)
    //  //  case None => Extension(
    //  //    extTVars,
    //  //    Param("p",paramType),
    //  //    Map()
    //  //  )
    //  //Some(res)
    //
    //protected def updExtMethods(l:LocalAPI):Map[String,ExtMethod] =
    //  val localNames  = l.apiClass.methods.keys.toSet
    //  val globalNames = methods.keys.toSet
    //  val newNames    = localNames.diff(globalNames)
    //  val nm1         = for (n,m) <- extension.methods yield (n,m.updStatement(localNames))
    //  val nm2         = mkGlobalMethods(
    //    l.apiClass.methods.filter(m=>newNames.contains(m._1)),l.apiClass.name)
    //  nm1++nm2
    //
    //protected def mkGlobalMethods(methods: Map[String,Method],className:String):Map[String,ExtMethod] =
    //  for (n,m) <- methods yield n->mkGlobalMethod(n,m,className)
    //
    //protected def mkGlobalMethod(name: String, method: Method,className:String):ExtMethod =
    //  ExtMethod(name,
    //    Nil,
    //    method.ev.map(e=>e.updKeysPrefix(className)),
    //    MethodCall(name,Nil,s"p._2.$name"::Nil),
    //    None
    //  )
    //
    ////protected def updMethodsFrom(l:LocalAPI):Extension =
    ////  var nmethods:Map[String,ExtMethod] = Map()
    ////  for m <- l.localClass.methods do
    ////    if methods.contains(m.name) then
    ////      nmethods += m.name -> methods(m.name).updEvidence(m.ev).updStatmentArg(m.name)
    ////
    ////    else
    ////      ???
    ////
    ////  this.copy(mehtods = nmethods)

  // Parameter
  case class Param(name:String,typ:TExp) extends Code:
    def toCode(implicit i:Int):String =
      ind(i) ++ name ++ ":" ++ typ.toString

    def withType(t:TExp) = this.copy(typ=t)
  // types
  trait TExp extends Code:
    def size():Int = this match
      case TName(_,_) => 1
      case TTuple(l) => l.size


  case class TName(name:String,typVars:Option[List[String]]=None) extends TExp:
    def toCode(implicit i:Int):String = ind(i) ++ (typVars match
      case Some(vars) => name++"["++vars.mkString(",")++"]"
      case None => name)

  case class TTuple(typs:List[TExp]) extends TExp:
    def toCode(implicit i:Int):String =
      ind(i) ++ typs.mkString("(",",",")")

  // method
  class Method(
    val name:String,
    val params:List[Param],
    val ev:Set[Evidence],
    val statement: Statement,
    val returnType:Option[TExp]) extends Code:

    def toCode(implicit i:Int):String =
      ind(i) ++
        s"""def $name${params(params.map(_.toString))}${mkTExp()} =\n""" ++
        statement.toCode(i+1)

    protected def mkTExp():String =
      if returnType.isDefined then
        ": " ++ returnType.get.toString
      else ""

  class ExtMethod(
    override val name:String,
    override val params:List[Param],
    override val ev:Set[Evidence],
    override val statement: MethodCall,
    override val returnType:Option[TExp]
  ) extends Method(name,params,ev,statement,returnType):

    override def toCode(implicit i:Int):String =
      ind(i) ++
        s"""def $name${params}${mkEvidence()(i+1)}${mkTExp()} =\n""" ++
        statement.toCode(i+1)

    protected def mkEvidence()(implicit i:Int):String =
      val evStr = ev.map(_.toString).toList
      val lg    = length(evStr)
      if ev.isEmpty then ""
      else if lg <40 then
        s"""(using ev: ${params(ev.map(_.toString).toList," | ",ln = false)(i+1)})"""
      else
        "(\n" ++ ind(i) ++ s"""using ev: ${params(ev.map(_.toString).toList," | ")(i+1)}\n"""++ s"${ind(Integer.max(i-1,0))})"
      //  "(\n" ++ ind(i) ++ s"""using ev: (\n${ev.map(e=>e.toCode(i+1)).mkString(s" |\n")}\n""" ++
      //ind(i)++")" ++ s"\n${ind(Integer.max(i-1,0))})"


  // Evidence
  case class Evidence(evidence:Map[String,String]) extends Code:
    def toCode(implicit i: Int): String =
      if evidence.nonEmpty then
        ind(i) ++
          showEv(evidence.keys.toList) ++
          "<:<" ++ showEv(evidence.values.toList)
      else ""

    protected def showEv(args:List[String]):String =
      if args.size == 1 then args.head
      else args.mkString("(",",",")")

    def updKeysPrefix(prefix:String):Evidence =
      Evidence(evidence.map(e=>(prefix++e._1,e._2)))
  // Statements
  sealed trait Statement extends Code
  case class MethodCall(method:String,typVars:List[String],args:List[String]) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++
        s"""$method${brackets(typVars)}${params(args)}"""

  case class Match(matchVars:List[String],cases:List[Case]) extends Statement:
    def toCode(implicit i: Int): String =
      ind(i) ++ s"""(${params(matchVars, ln = false)}:@unchecked) match\n""" ++ cases.map(c=>c.toCode(i+1)).mkString("\n")

  // Case
  case class Case(pattern:List[String],patternTyp:List[String], output:MethodCall) extends Code:
    def toCode(implicit i:Int):String =
      ind(i) ++ s"""case ${params(pattern,ln = false)}:${params(patternTyp,ln = false)}=> $output"""

  // Match Type
  case class MatchTyp(name:String, typVars: List[String], cases:List[Case]) extends Code:
    def toCode(implicit i:Int):String =
      s"""${ind(i)}type $name[
         |${typVars.map(v=>s"${ind(i+1)}$v:Boolean").mkString(",\n")}
         |${ind(i)}] = ${typVars.mkString("(",",",")")} match
         |${cases.map(c=>c.toCode(i+1))}""".stripMargin