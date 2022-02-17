package choreo.api

import choreo.npomsets.{NPom2PNs, NPomset}
import choreo.npomsets.NPom2PNs.*
import choreo.petrinet.PN
import choreo.petrinet.PN.*
import choreo.syntax.Agent
import choreo.syntax.Choreo.{In, Out}

/**
 * Created by guillecledou on 14/02/2022
 *
 * Petri net to Scala API
 */
@deprecated
object SetAPI:

  case class LocalAPI(name:String,
                      typVars:List[String],
                      params:List[Param],
                      methods:List[Method],
                      matchTyps:List[MatchTyp]):
    def code():String = ""

  case class GlobalAPI(name:String,
                       typVars:List[String],
                       params:List[Param],
                       methods:List[Method]):

    override def toString: String = code()

    def code():String =
      implicit val i = 0
      s"""
         |object $name:
         |
         |${mkExtension}
         |
         |""".stripMargin

    protected def mkExtension(implicit i:Int):String =
        s"""
         |${ind(i)}extension[
         |${typVars.map(v=>ind(i+1)++v++":Boolean").mkString(",\n")}
         |${ind(i)}](
         |${mkParams()(i+1)}
         |${ind(i)}){
         |${mkMethods()(i+1)}
         |${ind(i)}}
         |""".stripMargin

    protected def mkParams()(implicit i:Int):String =
      params.mkString(s"${ind(i)}",s",\n${ind(i)}","")
      //(for p <- params yield ind(i)++p.toString).mkString(",\n")

    protected def mkMethods()(implicit i:Int):String =
      (for m <- methods yield m.toCode).mkString(s"\n${ind(i)}")

  case class ScalaProtocol(global:GlobalAPI,
                           local: List[LocalAPI]):
    def code():Map[String,String] = Map()

  // types
  trait Typ
    //override def toString: String = this.toString

  case class TypName(name:String,typVars:Option[List[String]]=None) extends Typ:

    def toCode(implicit i:Int):String = ind(i) ++ (typVars match
      case Some(vars) => name++"["++vars.mkString(",")++"]"
      case None => name)

    override def toString = toCode(0)

  case class TypTuple(typs:List[Typ]) extends Typ:
    override def toString: String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++ typs.mkString("(",",",")")

  // parameter
  case class Param(name:String,typ:Typ):
    override def toString: String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++ name ++ ":" ++ typ.toString

  // method
  case class Method(name:String,
                    params:List[Param],
                    ev:Set[Evidence],
                    block:Block,
                    returnType:Option[Typ]):
    override def toString:String = toCode(0)


    def toCode(implicit i:Int):String =
      ind(i) ++
        s"""def $name${params}${mkEvidence()}${mkTyp()} =\n""" ++
        block.toCode(i)

    protected def mkEvidence():String =
      if ev.nonEmpty then
        s"""(using ev: ${ev.mkString("(","|",")")})"""
      else ""

    protected def mkTyp():String =
      if returnType.isDefined then
        returnType.get.toString
      else ""

  // todo: change to map[string,string]
  case class Evidence(vars:List[String],values:List[String]):
    override def toString:String =
      if vars.nonEmpty then
        if vars.size > 1 then vars.mkString("(",",",")") ++ "<:<" ++ values.mkString("(",",",")")
        else vars.mkString(",") ++ "<:<" ++ values.mkString(",")
      else ""

  trait Block:
    def toCode(implicit i:Int):String
  case class Match(matchVars:List[String],cases:List[Case]) extends Block:
    override def toString:String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++
        s"""${matchVars.mkString("(",",",")")} match\n""" ++
        cases.map(c=>c.toCode(i+1)).mkString("\n")


  case class New(scalaTyp: Typ, params:List[String]) extends Block:
    override def toString:String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++ scalaTyp.toString ++ params.mkString("(",",",")")

  case class SetCall(methods:List[String]) extends Block:
    override def toString:String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++ methods.mkString("(",",",")")

  case class Case(pattern:List[String],patternTyp:Typ, output:Typ):
    override def toString:String = toCode(0)

    def toCode(implicit i:Int):String =
      ind(i) ++ s"""case $pattern:$patternTyp => $output"""

  case class MatchTyp(name:String, typVars: List[String], cases:List[Case]):
    override def toString: String = toCode(0)

    def toCode(implicit i:Int):String =
        s"""${ind(i)}type $name[
           |${typVars.map(v=>s"${ind(i+1)}$v:Boolean").mkString(",\n")}
           |${ind(i)}] = ${typVars.mkString("(",",",")")} match
           |${cases.map(c=>c.toCode(i+1))}""".stripMargin

  ///////

  protected def  ind(i:Int):String = " "*i*2

  def apply(npom:NPomset):Map[Agent,ScalaProtocol] =
    // get petri nets
    val agentPNs = NPom2PNs(npom)
    val locals   = for (a,pns) <- agentPNs yield a->mkLocals(a,pns)
    val global   = for (a,ls) <- locals yield a->mkGlobal(a,ls)

    (for a <- npom.agents yield a->ScalaProtocol(global(a),locals(a))).toMap

  def global(a:Agent):String = s"Prot${a.s}"

  def mkGlobal(a:Agent,locals:List[LocalAPI]):GlobalAPI =
    val name                  = global(a)
    val typVars:List[String]  = for l <- locals ; t <- l.typVars yield name++t
    val params:List[Param]    = mkGlobalParams(locals)
    val methods:List[Method]  = mkGlobalMethods(locals)

    GlobalAPI(name, typVars, params, methods)

  def mkGlobalParams(locals:List[LocalAPI]):List[Param] =
    Param("p", TypTuple(for l <- locals yield TypName(l.name,Some(l.typVars))))::Nil

  def mkGlobalMethods(locals:List[LocalAPI]):List[Method] =
    val method2Local    = for l <- locals ; m <- l.methods yield (m.name,l)
    val localsByMethod  = method2Local.groupMap(_._1)(_._2)
    for (m,l) <- localsByMethod.view.toList yield mkGlobalMethod(m,l,locals)

  def mkGlobalMethod(name:String, locals:List[LocalAPI], all:List[LocalAPI]):Method =
    var params:List[Param]      = Nil // todo: maybe send(to:A,msg:Msg)... or receive(from:A,msg:Msg) ...
    var ev:Set[Evidence]        = mkGlobalEvidence(name,locals)
    var block:Block             = mkGlobalBlock(name,all)

    Method(name,params,ev,block,None)

  //def mkGlobalBegin(locals:List[ScalaLocalAPI]):Method =
  //  val block = for l <- locals yield
  //  Method("begin",List(),Set(),TypTuple(),None)

  def mkGlobalEvidence(name:String, locals:List[LocalAPI]):Set[Evidence] =
    (for l <- locals ; m <- l.methods ; if m.name == name yield
      for e <- m.ev yield Evidence(e.vars.map(v=>name++v),e.values)).flatten.toSet

  def mkGlobalBlock(name:String,all:List[LocalAPI]):Block =
    val methods =
      for (l,i) <- all.zipWithIndex yield
        if l.methods.find(m=>m.name == name).isDefined then
          s"p._$i.$name()"
        else s"p._$i.end()"
    SetCall(methods)

  def mkLocals(a:Agent,pns:Set[PN]):List[LocalAPI] =
    val groupCh = for pn <- pns yield pn.transitions.map(_.channel)
    val repeated:Set[In|Out] = groupCh.tail.foldRight(groupCh.head)(_.intersect(_))
    for pn <- pns.view.toList yield mkLocal(a,pn)(repeated)

  def mkLocal(a:Agent,pn:PN)(implicit repeated:Set[In|Out]):LocalAPI =
    val places:List[PlaceId]                      = pn.places.map(_.id).toList
    val typVars:List[String]                      = places.map(p=>typVar(p))
    val params:List[Param]                        = mkClassParams(places)
    val methodsMT:List[(Method,Option[MatchTyp])] = mkMethods(a,pn)
    val methods:List[Method]                      = methodsMT.map(_._1)
    val matchTyps:List[MatchTyp]                  = for (p,opt) <- methodsMT; mt <- opt yield mt

    LocalAPI(pn.name,typVars,params,methods,matchTyps)

  def mkClassParams(places:List[PlaceId]):List[Param] =
    for p <- places yield Param(typVarParam(p),TypName("Boolean"))

  def mkMethods(a:Agent,pn:PN)(implicit repeated:Set[In|Out]):List[(Method,Option[MatchTyp])] =
    val transByLabel:Map[In|Out,Set[Trans]] = pn.transitions.groupBy(_.channel)
    for (ch,trs) <- transByLabel.view.toList yield mkMethod(ch,trs)(a,pn,repeated)

  def mkMethod(ch:In|Out,trs:Set[Trans])(implicit a:Agent,pn:PN,repeated:Set[In|Out]): (Method,Option[MatchTyp]) =
    var params:List[Param]      = Nil // todo: maybe send(to:A,msg:Msg)... or receive(from:A,msg:Msg) ...
    var ev:Set[Evidence]        = mkTransEvidence(trs)
    //var block:Block             = mkBlock(trs)
    var returnType:Option[Typ]  = Some(mkReturnType(trs))
    var mTyp:Option[MatchTyp]   = None
    val places                  = pn.places.map(p=>p.id).toList

    var block:Block =
      if trs.size > 1 then
        if repeated.contains(ch) then
          val m = Match(places.map(typVarParam),mkCases(trs))
          mTyp = Some(MatchTyp(matchType(trs),m.matchVars,m.cases))
          m
        else
          val m = Match(places.map(typVarParam),mkCases(trs) appended mkEndCase(trs))
          mTyp = Some(MatchTyp(matchType(trs),m.matchVars,m.cases))
          m
      else
        val t = trs.head
        val output = t.pre.map(p=>p->"false").toMap ++ t.post.map(p=>p->"true")
        New(TypName(pn.name,Some(places.map(p=>output.getOrElse(p,typVar(p))))),
          places.map(p=>output.getOrElse(p,typVarParam(p))))

    (Method(method(ch),params,ev,block,returnType),mTyp)

  def mkCases(trs:Set[Trans])(implicit a:Agent,pn:PN):List[Case] =
    val trsList = trs.toList
    val disjoint = mkDisjointCases(trsList)
    for t <- trsList  yield mkCase(t,disjoint(t))

  def mkCase(t:Trans,pre:Map[String,String])(implicit a:Agent,pn:PN):Case =
    val typVars   = pn.places.map(p=>typVar(p.id)).toList
    val pattern   = for t <- typVars yield if pre.isDefinedAt(t) then s"_:${pre(t)}" else "_"
    val preVars   = typVars.map(v=>pre.getOrElse(v,"_"))
    var in:Typ    = TypName(pn.name,Some(preVars))
    var out:Typ   = mkReturnType(t)
    Case(pattern,in, out)

  def mkDisjointCases(trs:List[Trans])(implicit pn:PN): Map[Trans, Map[String, String]] =
    val preVars = trs.flatMap(t=>t.pre)
    val typVars = pn.places.map(_.id).toList
    (for t <- trs yield
      var option:Map[String,String] = Map()
      for (v <- typVars ; if t.pre.contains(v)) do
        option += (typVar(v) -> "true")
      for (v <- preVars ; name = typVar(v); if ! option.isDefinedAt(name)) do
        option += (name -> "false")
      (t->option)).toMap

  def mkEndCase(trs:Set[Trans])(implicit pn:PN):Case =
    val prePlaces = trs.flatMap(t=>t.pre)
    val places    = pn.places.map(_.id).toList
    val pre       = for p <- places yield if prePlaces.contains(p) then "false" else typVar(p)
    Case(pre.map(p=>"_"),TypName(pn.name,Some(pre)),TypName(pn.name,Some(places.map(p=>"false"))))

  def mkTransEvidence(trs:Set[Trans]):Set[Evidence] =
    for t<-trs yield mkTransEvidence(t)

  def mkTransEvidence(t:Trans):Evidence =
    Evidence(t.pre.map(typVar).toList,t.pre.map(_=>"true").toList)

  def mkReturnType(trs:Set[Trans])(implicit pn:PN) :Typ =
    val places = pn.places.map(p=>p.id).toList
    if trs.size == 1 then
      TypName(matchType(trs),Some(places.map(typVar)))
    else
      mkReturnType(trs.head)

  def mkReturnType(t:Trans)(implicit pn:PN) :Typ =
    val places = pn.places.map(p=>p.id).toList
    val output = t.pre.map(p=>p->"false").toMap ++ t.post.map(p=>p->"true")
    TypName(pn.name,Some(places.map(p=>output.getOrElse(p,typVar(p)))))

  protected def typVar(i:Int):String = "P"+i
  protected def typVarParam(i:Int):String = "p"+i

  protected def method(c:In | Out):String = c match
    case Out(a,b,msg) => s"""to_${b.s}_${msg.names}"""
    case In(a,b,msg) => s"""from_${a.s}_${msg.names}"""

  protected def matchType(trs:Set[Trans]):String = "T_"+trs.map(_.id).mkString("_")