package choreo.petrinet

import choreo.petrinet.PN.{Arc, NodeId, PlaceId, TransId, Trans, Place}
import choreo.petrinet.PN.Arc.{BGet, NBGet, NBGen}
import choreo.pomsets.Label
import choreo.syntax.{Agent, Msg}
import choreo.syntax.Choreo.{In, Out}

/**
 * Created by guillecledou on 30/01/2022
 */

/**
 * 1-Safe Petri Net
 * @param places set of places IDs
 * @param transitions a map from transitions ID to transitions
 * @param arcs set of arcs
 * @param marking the current marking of the net
 */
case class PN(places: Set[Place],
                transitions: Set[Trans],
                //arcs: Set[Arc],
                arcs: Set[Arc],
                marking: Set[PlaceId]):

  def pre(trans: Trans):Set[NodeId] =
    arcs.collect({case NBGet(p,t) if t == trans.id => p; case BGet(p,t) if t == trans.id => p})

  def post(trans: Trans):Set[NodeId] =
    arcs.collect({case NBGen(p,t) if t == trans.id => p})

  def preTransArcs(trans: Trans):Set[Arc] =
    arcs.collect({case a@NBGet(p,t) if t == trans.id => a; case a@BGet(p,t) if t == trans.id => a})

  def postTransArcs(trans: Trans):Set[Arc] =
    arcs.collect({case a@NBGen(p,t) if t == trans.id => a})


object PN:

  type PlaceId = Int
  type TransId = Int
  type NodeId = Int


  /** 1-Safe Petri Net Arc */
  trait Arc:

    val place,trans: Int

  object Arc:
    /** Non-Blocking generator arc */
    case class NBGen(place: Int, trans:Int) extends Arc
    /** Blocking consumer arc */
    case class BGet(place: Int, trans:Int) extends Arc
    /** Non-Blocking consumer arc */
    case class NBGet(place: Int, trans:Int) extends Arc

  case class Trans(id:Int, channel:In | Out, events:Set[String])
  case class Place(id:Int)

  // examples
  val a = Agent("a")
  val b = Agent("b")
  val hi = Msg(List("hi"))
  val ask = Msg(List("ask"))
  val done = Msg(List("done"))
  val yes = Msg(List("yes"))
  val no = Msg(List("no"))
  val pn1 = PN((1 to 11).map(Place(_)).toSet
    , Set(Trans(1,Out(a,b,hi),Set("1"))
      , Trans(2,In(b,a,hi) ,Set("2"))
      , Trans(3,Out(a,b,ask),Set("3","6","8"))
      , Trans(4,In(b,a,yes),Set("4","7"))
      , Trans(5,In(b,a,done),Set("5"))
      , Trans(9,In(b,a,no),Set("9"))
      , Trans(10,In(b,a,done),Set("10"))
    )
    ,
    Set(BGet(1,1)
      , NBGen(2,1)
      , NBGet(1,3)
      , BGet(2,2)
      , NBGen(3,2)
      , BGet(3,5)
      , BGet(4,3)
      , BGet(7,3)
      , NBGet(7,1)
      , BGet(9,3)
      , NBGet(9,1)
      , NBGen(5,3)
      , NBGen(8,3)
      , NBGen(10,3)
      , BGet(5,4)
      , NBGet(5,9)
      , BGet(8,4)
      , NBGet(8,9)
      , NBGet(10,4)
      , BGet(10,9)
      , NBGen(4,6)
      , NBGen(11,9)
      , BGet(6,5)
      , BGet(11,10))
    , Set(1,4,7,9)
  )