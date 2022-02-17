package choreo.api

import choreo.petrinet.OneSafeColouredPN
import choreo.petrinet.OneSafeColouredPN.*
//import choreo.petrinet.PN.Arc.*
import choreo.syntax.Choreo.{In, Out}
import choreo.api.API.*

import java.io.{File, FileWriter}

/**
 * Created by guillecledou on 30/01/2022
 *
 * One safe coloured petri net to Scala API
 */

//case class ScalaProtocol(apis:Set[ScalaAPIEnd])
//
//case class ScalaApiEnd(name:String,typeVars:List[String],methods:Set[ScalaApiMethod])
//case class ScalaApiMethod(name:String,params:List[ScalaParam],ev:Evidence,block:ScalaApiBlock,returnType:Option[ScalaType])
//case class Evidence(vars:List[String],values:Set[Set[String]])
//case class ScalaType(name:String,typeVars:Option[List[String]])
//case class ScalaParam(name:String,typ:ScalaType)


//object ScalaProtocol:
//
//  def apply(name:String, pn:PN):ScalaApiEnd =
//    val typeVars = pn.places.map(_.id).toList.sorted
//
//    // group transitions by same channel name
//    val groups:Map[In|Out,List[Trans]] =
//      for (ch,trs) <- pn.transitions.groupBy(k=>k.channel) yield
//        (ch,trs.toList)
//
//    val methods = mkMethods(groups)(pn,typeVars)
//
//    ScalaApiEnd(name, typeVars, methods)
//
//    protected def mkMethods(groups:Map[In|Out,List[Trans]])(implicit pn:PN,typeVars:List[Int]):Iterable[Method] =
//      for (ch,trs) <- groups yield mkMethod(ch, trs)
//
//    protected def mkMethod(ch:In|Out,trs:List[Trans])(implicit pn:PN,typeVars:List[Int]):Method =
//      val preBPlaces = trs.flatMap(t=> pn.preTransArcs(t)).collect({case BGet(f,_)=>f})
//      val evidence = mkEvidence(preBPlaces,trs.map(t=>preCond(t)).toSet)
//      trs match
//        case t::Nil =>
//          val postType = for v<-typeVars yield
//            if postCond(t).isDefinedAt(v) then postCond(t)(v).toString else typeVarName(v)
//          ind(i) ++
//            s"""def ${methodName(ch)}$evidence = """ ++ "\n" ++
//            ind(i+1) ++ s"new $name[${postType.mkString(",")}]"
//        case t::ts =>
//          ind(i) ++
//            s"""inline def ${methodName(ch)}$evidence"""++ "\n" ++
//            ind(i+1) ++ s""":${matchTypeName(trs)}[$typeVarsStr] = """ ++ s"inline this match" ++ "\n" ++
//            mkCases(trs)(using i + 1).mkString("\n")

//todo: separate api generation from code generation
//      check disjoint cases
@deprecated
case class API(name:String,pn:OneSafeColouredPN):
  lazy val typeVars = pn.places.map(_.id).toList.sorted.map(typeVarName)
  //lazy val typeVarsStr = typeVars.mkString(",")
  lazy val preCond = (for t <- pn.transitions yield t->preConditions(t)).toMap
  lazy val postCond = (for t <- pn.transitions yield t->postConditions(t)).toMap
  protected def  ind(i:Int):String = " "*i*2

  override def toString: String =  mkCode()(using 0)

  def save(path:String):Unit =
    val fileWriter = new FileWriter(new File(path))
    fileWriter.write(this.toString)
    fileWriter.close()

  def mkCode()(using i:Int):String =
    val groups = for (ch,trs) <- pn.transitions.groupBy(k=>k.channel) yield
      (ch,trs.toList)
    ind(i) ++
      s"""
       |package api
       |
       |import $name.*
       |
       |class $name[${typeVars.mkString(",")}] private:
       |
       |${mkMethods(groups)(using i+1).mkString("\n\n")}
       |
       |object $name:
       |  type Init = $name[${mkInit().mkString(",")}]
       |  type Final =$name[${typeVars.map(_=>"false").mkString(",")}]
       |
       |  def apply[${typeVars.mkString(",")}]() = new $name[${typeVars.mkString(",")}]
       |  // todo: shell for initially enabled methods
       |
       |${mkMatchTypes(groups)(using i+1).mkString("\n\n")}
       |""".stripMargin

  def mkInit():List[String] =
    val init = pn.marking.map(typeVarName)
    for v <- typeVars yield if init.contains(v) then "true" else "false"

  protected def mkMethods(groups:Map[In|Out,List[Trans]])(using i:Int):Iterable[String] =
    for (ch,trs) <- groups yield mkMethod(ch, trs)

  protected def mkMethod(ch:In|Out,trs:List[Trans])(using i:Int):String =
    val prePlaces = trs.flatMap(t=> t.prePlaces).map(typeVarName)
    val evidence = mkEvidence(prePlaces,trs.flatMap(t=>preCond(t)).toSet) // assumes transitions with same label have disjoint pre places
    trs match
      case t::Nil =>
        val postType = for v<-typeVars yield
          if postCond(t).isDefinedAt(v) then postCond(t)(v) else v
        ind(i) ++
          s"""def ${methodName(ch)}$evidence = """ ++ "\n" ++
          ind(i+1) ++ s"new $name[${postType.mkString(",")}]"
      case t::ts =>
        ind(i) ++
          s"""inline def ${methodName(ch)}$evidence"""++ "\n" ++
          ind(i+1) ++ s""":${matchTypeName(trs)}[${typeVars.mkString(",")}] = """ ++ s"inline this match" ++ "\n" ++
          mkCases(trs)(using i + 1).mkString("\n")

  protected def mkMatchTypes(groups:Map[In|Out,List[Trans]])(using i:Int):Iterable[String] =
    for (ch,trs) <- groups if trs.size > 1 yield mkMatchType(trs)

  protected def mkMatchType(trs:List[Trans])(using i:Int):String =
    ind(i) ++
      s"""type ${matchTypeName(trs)}[${typeVars.mkString(",")}] = (${typeVars.mkString(",")}) match""" ++ "\n" ++
      mkTypeCases(trs)(using i+1).mkString("\n")

  protected def mkTypeCases(trs:List[Trans])(using i:Int):List[String] =
    val disjointCases = mkDisjointCases(typeVars,trs)
    for t <- trs ; pre <- disjointCases(t) yield
      var preType,postType:List[String] = Nil
      for v <- typeVars do
        preType :+= (if pre.isDefinedAt(v) then pre(v) else "_")
        postType :+= (if postCond(t).isDefinedAt(v) then postCond(t)(v) else v)

      ind(i) ++ s"""case (${preType.mkString(",")}) => $name[${postType.mkString(",")}]"""

  protected def mkCases(trs:List[Trans])(using i:Int):List[String] =
    val disjointCases = mkDisjointCases(typeVars,trs)
    for t <- trs ; pre <- disjointCases(t) yield
      var preType,postType:List[String] = Nil
      for v <- typeVars do
        preType :+= (if pre.isDefinedAt(v) then pre(v) else "_")
        postType :+= (if postCond(t).isDefinedAt(v) then postCond(t)(v) else v)

      ind(i) ++ s"""case _:$name[${preType.mkString(",")}] => $name[${postType.mkString(",")}]()"""

  protected def mkEvidence(places:List[String], pre:Set[Map[String,String]]):String =
    val alternatives = for c <- pre yield mkOneEvidence(places,c)
    s"""(using ev: ${places.mkString("(",",",")")} <:< ${alternatives.mkString("(","|",")")})"""

  protected def mkOneEvidence(places:List[String], pre:Map[String,String]):String =
    (for p <- places yield if pre.isDefinedAt(p) then pre(p) else "Boolean").mkString("(",",",")")

  //protected def preConditions(t:Trans):MapVar =
  //  pn.preTransArcs(t).collect({case BGet(from, to) => from -> true}).toMap

  protected def preConditions(t:Trans):Set[Map[String,String]] =
    for (opt <- t.pre.options) yield
      opt.map({case Var(n) => typeVarName(n) -> "true" ; case NVar(n) => typeVarName(n) -> "false"}).toMap

  protected def postConditions(t:Trans):Map[String,String] =
    (for (p,v) <- t.post yield typeVarName(p)-> {v match {
      case n:PlaceId => typeVarName(n)
      case b:Boolean => b.toString
    }}) ++
      (for (p <- t.prePlaces) yield typeVarName(p) -> "false").toMap // too fixed?


  //protected def postConditions(t:Trans):MapVar =
  //  (pn.preTransArcs(t)++pn.postTransArcs(t)).collect({
  //    case BGet(p, _) => p -> false
  //    case NBGet(p, _) => p -> false
  //    case NBGen(p, _) => p -> true
  //  }).toMap

  protected def mkDisjointCases(typeVars: List[String], trs:List[Trans]): Map[Trans, Set[Map[String, String]]] =
    val preVars = trs.flatMap(t=>t.prePlaces)

    (for ( (t,pre) <- trs.map(t=>(t,preCond(t)))) yield
      var options:Set[Map[String,String]] = Set()
      for (opt <- pre) do
        var option:Map[String,String] = Map()
        for (v <- typeVars ; if opt.isDefinedAt(v)) do
          option += (v -> opt(v))
        for (v <- preVars ; name = typeVarName(v); if ! option.isDefinedAt(name)) do
          option += (name -> "false")
        options += option
      (t->options)).toMap

  protected def typeVarName(i:Int):String = "P"+i

  protected def methodName(c:In | Out):String = c match
    case Out(a,b,msg) => s"""to_${b.s}_${msg.names}"""
    case In(a,b,msg) => s"""from_${a.s}_${msg.names}"""

  protected def matchTypeName(trs:List[Trans]):String = "T_"+trs.map(_.id).mkString("_")

object API:
  //type MapVar = Map[Int,Boolean]