package choreo.realisability

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Order,Event, add,invert,toPair}
import choreo.realisability.Interclosure
import choreo.datastructures.DAG
import choreo.datastructures.Isomorphism
import choreo.datastructures.Isomorphism._

import cats.implicits._

/**
 * Created by guillecledou on 06/07/2021
 *
 * Emilio's definition of realisability, adapted to nested pomsets
 */
object NPomERealisability:
  type CC2Result = Either[String,Isomorphisms[Event,Event]]

  case class CC2Evidence(ic:Order,icId:Int,result:CC2Result):
    override def toString: String = result match
      case Left(err) => s"""Interclosure $icId does not satisfy CC2:\n$err\n"""
      case Right(isos) =>
        //s"""Interclosure $icId satisfies CC2 """ ++
        s"""Interclosure $icId with arrows:
           |${toPair(invert(ic)).mkString(",")}
           |""".stripMargin++
        s"""satisfies CC2 """ ++
        s"""with the following isomorphisms:
           |${isos.map(iso=>iso.mkString(",")).mkString("\n")}\n""".stripMargin

  /**
   * Checks if a nested pomset is realisable
   * using and adaptation of Emilio's definitions
   * @param p nested pomset
   * @return a message specifying success or why it failed
   */
  //def apply(pom:NPomset):RResult = cc2(pom)


  /**
   * Checks CC2-POM condition over a nested pomset
   * @param pom nested pomset
   * @return a message specifying success or why it failed
   */
  def cc2(pomset:NPomset):List[CC2Evidence] =
    val pom = pomset.simplified
    val (proj,ics) = pom.einterclosure
    val projections = proj.map(p=>p.simplified)

    val gg = DAG.fromPred(pom.events.toSet,pom.pred)
    val lg = DAG.fromPred(pom.events.toSet,
      projections.map(p=>p.pred).foldRight[Order](Map())({case (a,n) => add(n,a)}))

    for (ic,id) <- ics.zipWithIndex yield
      CC2Evidence(ic,id, isIsomorphic(gg,lg++invert(ic),(n1,n2) => pom.actions(n1) == pom.actions(n2)))
    //lgs.traverse(lg=>isIsomorphic(gg,lg,(n1,n2) => pom.actions(n1) == pom.actions(n2))) match
    //  case Left(err) => Left(err)
    //  case Right(listSet) => Right(listSet.flatten.toSet)

  protected def isIsomorphic(g1:DAG[Event], g2:DAG[Event], f:((Event,Event))=>Boolean):CC2Result =
    Isomorphism.isIsomorphic(g1,g2,f) match
      case Some(iso) => Right(iso)
      case None => Left(s"""their orders are not isomorphic\n-global:${g1}\n-local:${g2}""")

/**
   * Checks CC3-POM condition over a nested pomset
   * @param p nested pomset
   * @return a message specifying success or why it failed
   */
  //def cc3(p:NPomset):String =
  //  // global pomset
  //  val pom = p.simplified
  //  val gg = DAG.fromPred(pom.events.toSet,pom.pred)
  //  val prefixesGG = gg.prefixDAGs()
  //  // local projections
  //  val proj = pom.projectAll
  //  val
  //
  //  if isIsomorphic(gg,lg,(n1,n2) => pom.actions(n1) == pom.actions(n2))
  //
  //  ???
  //
  //def prefixes(projs:NPom)


  ///**
  // * Checks if a nested pomset is termination sound
  // * @param p pomset
  // * @return a message specifying success or why it failed
  // */
  //def termination(p:NPomset):RResult = ???