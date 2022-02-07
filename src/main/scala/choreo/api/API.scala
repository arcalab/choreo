package choreo.api

import choreo.petrinet.PN
import choreo.petrinet.PN.*
import choreo.petrinet.PN.Arc.*
import choreo.syntax.Choreo.{In, Out}
import choreo.api.API.*

import java.io.{File, FileWriter}

/**
 * Created by guillecledou on 30/01/2022
 */

//todo: separate api generation from code generation
//      check disjoint cases
case class API(name:String,pn:PN):
  lazy val typeVars = pn.places.map(_.id).toList.sorted
  lazy val typeVarsStr = typeVars.map(typeVarName).mkString(",")
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
       |class $name[$typeVarsStr] private:
       |
       |${mkMethods(groups)(using i+1).mkString("\n\n")}
       |
       |object $name:
       |
       |${mkMatchTypes(groups)(using i+1).mkString("\n\n")}
       |""".stripMargin

  protected def mkMethods(groups:Map[In|Out,List[Trans]])(using i:Int):Iterable[String] =
    for (ch,trs) <- groups yield mkMethod(ch, trs)

  protected def mkMatchTypes(groups:Map[In|Out,List[Trans]])(using i:Int):Iterable[String] =
    for (ch,trs) <- groups if trs.size > 1 yield mkMatchType(trs)

  protected def mkMatchType(trs:List[Trans])(using i:Int):String =
    ind(i) ++
      s"""type ${matchTypeName(trs)}[${typeVarsStr}] = (${typeVarsStr})""" ++ "\n" ++
      mkTypeCases(trs)(using i+1).mkString("\n")

  protected def mkTypeCases(trs:List[Trans])(using i:Int):List[String] =
    for t <- trs yield
      var preType,postType:List[String] = Nil
      for v <- typeVars do
        preType :+= (if preCond(t).isDefinedAt(v) then preCond(t)(v).toString else "_")
        postType :+= (if postCond(t).isDefinedAt(v) then postCond(t)(v).toString else typeVarName(v))

      ind(i) ++ s"""case (${preType.mkString(",")}) = new $name[${postType.mkString(",")}]"""

  protected def mkMethod(ch:In|Out,trs:List[Trans])(using i:Int):String =
    val preBPlaces = trs.flatMap(t=> pn.preTransArcs(t)).collect({case BGet(f,_)=>f})
    val evidence = mkEvidence(preBPlaces,trs.map(t=>preCond(t)).toSet)
    trs match
      case t::Nil =>
        val postType = for v<-typeVars yield
          if postCond(t).isDefinedAt(v) then postCond(t)(v).toString else typeVarName(v)
        ind(i) ++
          s"""def ${methodName(ch)}$evidence = """ ++ "\n" ++
          ind(i+1) ++ s"new $name[${postType.mkString(",")}]"
      case t::ts =>
        ind(i) ++
          s"""inline def ${methodName(ch)}$evidence"""++ "\n" ++
          ind(i+1) ++ s""":${matchTypeName(trs)}[$typeVarsStr] = """ ++ s"inline this match" ++ "\n" ++
          mkCases(trs)(using i + 1).mkString("\n")

  protected def mkCases(trs:List[Trans])(using i:Int):List[String] =
    for t <- trs yield
      var preType,postType:List[String] = Nil
      for v <- typeVars do
        preType :+= (if preCond(t).isDefinedAt(v) then preCond(t)(v).toString else "_")
        postType :+= (if postCond(t).isDefinedAt(v) then postCond(t)(v).toString else typeVarName(v))

      ind(i) ++ s"""case _:$name[${preType.mkString(",")}] = $name[${postType.mkString(",")}]"""

  protected def mkEvidence(places:List[Int], pre:Set[MapVar]):String =
    val alternatives = for c <- pre yield mkOneEvidence(places,c)
    s"""(using ev: ${places.map(typeVarName).mkString("(",",",")")} <:< ${alternatives.mkString("(","|",")")})"""

  protected def mkOneEvidence(places:List[Int], pre:MapVar):String =
    (for p <- places yield if pre.isDefinedAt(p) then "true" else "Boolean").mkString("(",",",")")

  protected def preConditions(t:Trans):MapVar =
    pn.preTransArcs(t).collect({case BGet(from, to) => from -> true}).toMap

  protected def postConditions(t:Trans):MapVar =
    (pn.preTransArcs(t)++pn.postTransArcs(t)).collect({
      case BGet(p, _) => p -> false
      case NBGet(p, _) => p -> false
      case NBGen(p, _) => p -> true
    }).toMap


  protected def typeVarName(i:Int):String = "P"+i

  protected def methodName(c:In | Out):String = c match
    case Out(a,b,msg) => s"""to_${b.s}_${msg.names}"""
    case In(a,b,msg) => s"""from_${a.s}_${msg.names}"""

  protected def matchTypeName(trs:List[Trans]):String = "T_"+trs.map(_.id).mkString("_")

object API:
  type MapVar = Map[Int,Boolean]