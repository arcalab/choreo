package choreo.petrinet

import choreo.petrinet.OneSafeColouredPN.{NodeId, Place, PlaceId, Trans, TransId}

import scala.collection.immutable.{AbstractSet, SortedSet}
//import choreo.petrinet.PN.Arc.{BGet, NBGen, NBGet}
//import choreo.petrinet.PN.BExp.*
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
@deprecated
case class OneSafeColouredPN(places: Set[Place],
                             transitions: Set[Trans],
                             //arcs: Set[Arc],
                             marking: Set[PlaceId],
                             name:String):

  // joins two petri nets
  def ++(other:OneSafeColouredPN):OneSafeColouredPN =
    val pids = this.places.map(_.id)
    val tids = this.transitions.map(_.id)
    val opids = other.places.map(_.id)
    val otids = other.transitions.map(_.id)

    if pids.intersect(opids).nonEmpty || tids.intersect(otids).nonEmpty then
      this.join(OneSafeColouredPN.rename(other)((pids++tids).max+1))
    else this.join(other)

  // joins two petri nets with no colliding ids for places or transitions
  protected def join(other:OneSafeColouredPN):OneSafeColouredPN =
    OneSafeColouredPN(this.places++other.places
      , this.transitions++other.transitions
      , this.marking++other.marking
      , this.name++other.name)


object OneSafeColouredPN:

  type PlaceId = Int
  type TransId = Int
  type NodeId = Int

  def apply():OneSafeColouredPN = OneSafeColouredPN(Set(),Set(),Set(),"")

  def rename(pn:OneSafeColouredPN)(implicit i:NodeId):OneSafeColouredPN =
    var seed = i
    def next() = {seed+=1; seed-1}

    val placesMap = pn.places.map(p=> p.id->next()).toMap
    val transMap = pn.transitions.map(t=> t.id->next()).toMap

    OneSafeColouredPN(pn.places.map(p=> Place(placesMap(p.id)))
      , rename(pn.transitions,placesMap,transMap)
      , pn.marking.map(p=>placesMap(p))
      , pn.name )

  protected def rename(trans:Set[Trans],pMap:Map[PlaceId,PlaceId],tMap:Map[TransId,TransId]):Set[Trans] =
    for t <- trans yield rename(t,pMap,tMap)

  protected def rename(t:Trans, pMap:Map[PlaceId,PlaceId], tMap:Map[TransId,TransId]):Trans =
    Trans(tMap(t.id),t.channel,rename(t.pre,pMap),rename(t.post,pMap))

  protected def rename(dnf:DNF[PlaceId],pMap:Map[PlaceId,PlaceId]):DNF[PlaceId] =
    val options:Set[Set[BVar[PlaceId]]] =
      for and <- dnf.options yield
        for v <- and yield
          v match
            case Var(n) => Var(pMap(n))
            case NVar(n)=> NVar(pMap(n))
    DNF(options)

  protected def rename(post:Fun[PlaceId,PlaceId],pMap:Map[PlaceId,PlaceId]):Fun[PlaceId,PlaceId] =
    for (p,exp) <- post yield
      (pMap(p),
        exp match
          case i:PlaceId => pMap(i)
          case b => b
      )


  ///** 1-Safe Petri Net Arc */
  //trait Arc:
  //
  //  val place,trans: Int
  //
  //object Arc:
  //  /** Non-Blocking generator arc */
  //  case class NBGen(place: Int, trans:Int) extends Arc
  //  /** Blocking consumer arc */
  //  case class BGet(place: Int, trans:Int) extends Arc
  //  /** Non-Blocking consumer arc */
  //  case class NBGet(place: Int, trans:Int) extends Arc

  //case class Trans(id:Int, channel:In | Out, events:Set[String])
  case class Place(id:Int/*,name:String*/)

  // todo: channel it to dependent, but if we want to access the sender, receiver, and msg, is the easiest way for now
  case class Trans(id:Int, channel:In|Out, pre:DNF[PlaceId], post:Fun[PlaceId,PlaceId]): // pre and post are ugly
    def prePlaces:Set[PlaceId] =
      this.pre.vars

    def postPlaces:Set[PlaceId] =
      this.post.keys.toSet

  type Fun[A,B] = Map[A, B | Boolean] //todo: change Set[B]

  case class DNF[A](options:Set[Set[BVar[A]]]):
    def vars:Set[A] = options.flatten.collect({case Var(n)=>n ; case NVar(n)=>n})


  trait BVar[A](name:A)
  case class Var[A](name:A)  extends BVar[A](name)
  case class NVar[A](name:A) extends BVar[A](name)



  //trait BExp[A]:
  //  def ||(other:BExp[A]) = Or(this,other)
  //  def &&(other:BExp[A]) = And(this,other)
  //  def not = Not(this)
  //  def ->(other:BExp[A]) = this.not || other
  //  def <->(other:BExp[A]) = this -> other && other -> this
  //
  //  def vars:Set[A] = this match {
  //    case Var(a) => Set(a)
  //    case And(l,r) => l.vars ++ r.vars
  //    case Or(l,r) => l.vars ++ r.vars
  //    case Not(a) => a.vars
  //    case True => Set()
  //  }
  //
  //  def dnf:BExp[A] =
  //    val once = this.nnf.dnfOnce
  //    if this != once then once.nnf.dnfOnce else once
  //
  //  protected def dnfOnce:BExp[A] = this match
  //    case Var(_) | Not(_) | True => this
  //    case And(l,r) => (l.dnfOnce,r.dnfOnce) match
  //      case (Or(l1,r1),r2) => (l1 && r2) || (r1 && r2)
  //      case (l,Or(l1,r1)) => (l && l1) || (l && r1)
  //      case (l1,l2) => l1 && l2
  //    case Or(l,r) => Or(l.dnfOnce,r.dnfOnce)
  //
  //  def nnf:BExp[A] =
  //    val once = this.nnfOnce
  //    if this != once then once.nnf else once
  //
  //  protected def nnfOnce:BExp[A] = this match
  //    case a:Var[A] => a
  //    case a1@Not(a) => a.nnfOnce match
  //      case _:Var[A] => a1
  //      case Not(a2) => a2.nnfOnce
  //      case And(l,r) => l.not || r.not
  //      case Or(l,r) => l.not && r.not
  //      case True => a1
  //    case And(l,r) => And(l.nnfOnce,r.nnfOnce)
  //    case Or(l,r) => Or(l.nnfOnce,r.nnfOnce)
  //    case True => True
  //
  //object BExp:
  //  case object True extends BExp[Nothing]
  //  case class Var[A](a:A) extends BExp[A]
  //  case class And[A](l:BExp[A], r:BExp[A]) extends BExp[A]
  //  case class Or[A](l:BExp[A], r:BExp[A]) extends BExp[A]
  //  case class Not[A](a:BExp[A]) extends BExp[A]




  // examples
  // from console:
  // import choreo.petrinet.* ; import PN.* ; import choreo.api.API
  // API("AgentA",pn1).save("gen/a.scala")
  //val a = Agent("a")
  //val b = Agent("b")
  //val hi = Msg(List("hi"))
  //val ask = Msg(List("ask"))
  //val done = Msg(List("done"))
  //val yes = Msg(List("yes"))
  //val no = Msg(List("no"))
  //val pn1 = PN((1 to 11).map(Place(_)).toSet
  //  , Set(Trans(1,Out(a,b,hi), DNF(Set(Set(Var(1)))),Map(2->true,7->false,9->false)) //Set("1"))
  //    , Trans(2,In(b,a,hi), DNF(Set(Set(Var(2)))),Map(3->true))//,Set("2"))
  //    , Trans(3,Out(a,b,ask),DNF(Set(Set(Var(4)),Set(Var(7)),Set(Var(9)))),Map(5->4,8->7,10->9))//Set("3","6","8"))
  //    , Trans(4,In(b,a,yes), DNF(Set(Set(Var(5)),Set(Var(8)))),Map(6->true,10->false))//Set("4","7"))
  //    , Trans(5,In(b,a,done),DNF(Set(Set(Var(3),Var(6)))),Map())//Set("5"))
  //    , Trans(9,In(b,a,no),DNF(Set(Set(Var(10)))),Map(5->false,8->false,11->true))//Set("9"))
  //    , Trans(10,In(b,a,done),DNF(Set(Set(Var(11)))),Map()) //Set("10"))
  //  )
  //  //,
  //  //Set(BGet(1,1)
  //  //  , NBGen(2,1)
  //  //  , NBGet(1,3)
  //  //  , BGet(2,2)
  //  //  , NBGen(3,2)
  //  //  , BGet(3,5)
  //  //  , BGet(4,3)
  //  //  , BGet(7,3)
  //  //  , NBGet(7,1)
  //  //  , BGet(9,3)
  //  //  , NBGet(9,1)
  //  //  , NBGen(5,3)
  //  //  , NBGen(8,3)
  //  //  , NBGen(10,3)
  //  //  , BGet(5,4)
  //  //  , NBGet(5,9)
  //  //  , BGet(8,4)
  //  //  , NBGet(8,9)
  //  //  , NBGet(10,4)
  //  //  , BGet(10,9)
  //  //  , NBGen(4,6)
  //  //  , NBGen(11,9)
  //  //  , BGet(6,5)
  //  //  , BGet(11,10))
  //  , Set(1,4,7,9)
  //  , "AgentA"
  //)