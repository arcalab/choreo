package choreo.realisability

import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Order,Event, add}
import choreo.realisability.Interclosure
import choreo.datastructures.Graph
import choreo.datastructures.Isomorphism
import choreo.datastructures.Isomorphism._

import cats.implicits._

/**
 * Created by guillecledou on 06/07/2021
 *
 * Emilio's definition of realisability, adapted to nested pomsets
 */
object NPomERealisability:
  type Isomorphism = PS[Event,Event]
  type CC2Result = Either[String,Set[Isomorphism]]

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
  def cc2(pom:NPomset):CC2Result=
    val (proj,ics) = pom.einterclosure
    val projs = proj.map(p=>p.simplified)

    val projOrder = projs.map(p=>p.pred).foldRight[Order](Map())({case (a,n) => add(n,a)})
    val global:Order = pom.pred
    val local:Set[Order] = ics.map(ic=> add(ic,projOrder))
    
    val gg = Graph.fromPred(pom.events.toSet,global)
    val lgs = local.map(l=>Graph.fromPred(pom.events.toSet,l)).toList

    lgs.traverse(lg=>isIsomorphic(gg,lg,(n1,n2) => pom.actions(n1) == pom.actions(n2))) match
      case Left(err) => Left(err)
      case Right(listSet) => Right(listSet.flatten.toSet)


  protected def isIsomorphic(g1:Graph[Event],g2:Graph[Event],f:((Event,Event))=>Boolean):CC2Result =
    Isomorphism.isIsomorphic(g1,g2,f) match
      case Some(iso) => Right(iso)
      case None => Left("Not isomorphic: \n" ++
        s"${g1}\n" ++
        s"w.r.t\n" +
        s"${g2}\n")

///**
  // * Checks CC3-POM condition over a nested pomset
  // * @param p nested pomset
  // * @return a message specifying success or why it failed
  // */
  //def cc3(p:NPomset):RResult = ???
  //
  ///**
  // * Checks if a nested pomset is termination sound
  // * @param p pomset
  // * @return a message specifying success or why it failed
  // */
  //def termination(p:NPomset):RResult = ???