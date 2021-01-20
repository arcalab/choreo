package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.SOS._
import choreo.choreo2.backend.Multiset._
import choreo.choreo2.backend.{LTS, Multiset}
import choreo.choreo2.analysis.System._


object Bisimulation :
  /////////////////////////////////////
  /// Full bisimulation experiments ///
  /////////////////////////////////////
  //  type AMultiset = Multiset[Action]
  //  type System = (Set[Choreo],AMultiset)
  //
  //  def initSys(c:Choreo): System =
  //    (allProj(c).values.toSet,Multiset())
  //
  //  def isFinal(s:System): Boolean =
  //    s._2.isEmpty && s._1.forall(c => canSkip(c))
  //
  //  def nextSys(c:Choreo) =
  //    next(initSys(c))
  //
  //  def next(s:System): Set[(Action,System)] =
  //    val x = for (proj <- s._1) yield  // get each projection
  //      //println(s"next proj in sys is $proj")
  //      val proj2 = evolveProj(proj,s._2) // get all evolutions = (act,newProj,newNet)
  //      //println(s" - got evolution: $proj2")
  //      val newProj = for ((act,p2,n2)<-proj2) yield
  //        (act, (s._1-proj+p2 , n2) )
  //      //println(s" - updated evolution: $newProj")
  //      newProj.toSet
  //    x.flatten
  //
  //  def evolveProj(c:Choreo, net:AMultiset): List[(Action,Choreo,AMultiset)] =
  //    for (act,chor)<-next(c) if allowed(act,net) yield
  //      (act,chor, act match {
  //        case In(a,b,m)  => net - Out(b,a,m)
  //        case Out(a,b,m) => net + Out(a,b,m)
  //      })
  //
  //  def allowed(act: Action, net: AMultiset): Boolean =
  //    act match {
  //      case In(a, b, m) => net contains Out(b,a,m)
  //      case Out(a, b, m) => true
  //    }
  //
  //
  //  ////
  //
  //  trait LTS[S<:Any](init:S):
  //    type St = S
  //    def trans: Set[(Action,LTS[S])]
  //    def accepting: Boolean
  //    def isEmpty: Boolean

  case class Global(c:Choreo) extends LTS[Choreo](c):
    def trans: Set[(Action,LTS[Choreo])] =
      for (a,c2) <- nextChoreo(c).toSet yield (a,Global(c2))
    def accepting: Boolean = canSkip(c)
    def isEmpty: Boolean = c==End // never stuck
    override def toString: String =
      s"Global[$c]"

  case class Local(s:System) extends LTS[System](s):
    def trans: Set[(Action,LTS[System])] =
      for (a,s2) <- next(s:System) yield (a,Local(s2))
    def accepting:Boolean = isFinal(s)
    def isEmpty: Boolean =
      s._2.isEmpty && s._1.forall(c => c==End)
    override def toString: String =
      s"Local[${s._1.mkString("][")}]<${s._2}>"


  type R[A,B] = Set[(A,B)]
  type RC = R[LTS[Choreo],LTS[System]]
  type ROld = R[Choreo,System]

  def findBisim(c:Choreo): RC =
    findBisim[Choreo,System](Set(),Set((Global(c),Local(initSys(c)))))

  def findBisimPP(c:Choreo): String =
    findBisim(c).map(p=>s"${p._1}  ><  ${p._2} (${p._1.accepting} >< ${p._2.accepting})").mkString("\n")

  /** Find a bisimulation.
   *  Not checking yet if both sides are final or none is final in each member. */
  def findBisim[G<:Any,L<:Any](visited:R[LTS[G],LTS[L]],
                               missing:R[LTS[G],LTS[L]],i:Int=1): R[LTS[G],LTS[L]] =
    //    println(s"[Sim] $visited  --  $missing")
    type S = R[LTS[G],LTS[L]]
    missing.headOption match
      case Some((g,l)) =>
        if visited contains (g,l) then
          findBisim(visited,missing-((g,l)),i)
        else
          val nxtG = g.trans // next(cs._1)
          val nxtL = l.trans // next(cs._2)
          println(s"[Sim] Round $i - doing ${(nxtG.map(_._1)++nxtL.map(_._1)).toSet.mkString(",")}")
//          if nxtL.isEmpty && !l.isEmpty then
//            println(s"[Sim] not a bisimulation:\n - $l is stuck but it is not done.")
//            return Set()

          // for every cs1 --a1-> cs1',
          //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
          var more: S = Set()

          // yin again
          getMatches(nxtG,nxtL) match
            case Left(a) =>
              println(s"[Sim] not a bisimulation:\n - $g can do $a\n - $l cannot")
              return Set()
            case Right(more1) => more = more ++ more1
          // yang
          getMatches(nxtL,nxtG) match
            case Left(a) =>
              println(s"[Sim] not a bisimulation:\n - $l can do $a\n - $g cannot")
              return Set()
            case Right(more2) => more = more ++ more2.map(_.swap)

          // check if newR has matching accepting states
          for (c,s)<-more do
            if (c.accepting != s.accepting) then
            println(s"[Sim] not a bisimulation:\n - $c ${
              if c.accepting then s"is accepting\n - $s is not"
              else s"is not accepting\n - $s is"}")
            // iterate
          findBisim(visited+((g,l)),(missing++more)-((g,l)),i+1)

      case None => visited // Success!


  // experiment: not being used yet.
  def getMatches[S1<:LTS[_],S2<:LTS[_]](steps1:R[Action,S1],
                                        steps2:R[Action,S2]): Either[Action,R[S1,S2]] =
    var more: R[S1,S2] = Set()
    for (a1,st1) <- steps1 do
      val next = steps2      // from the actions of `l`
        .filter(_._1==a1)    // get the ones that perform `act`
        .map(_._2)           // and collect the next state
      if next.isEmpty then
      //println(s"[Sim] not a bisimulation:\n - $g can do $a2\n - $l cannot")
        return Left(a1) // fail! (no match)
      else
        more = more ++ next.map((st1,_))
    Right(more)