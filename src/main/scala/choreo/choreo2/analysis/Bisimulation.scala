package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.Global
import choreo.choreo2.backend.Multiset._
import choreo.choreo2.backend.{LTS, Multiset}
import choreo.choreo2.analysis.{Local,LocalTau}


object Bisimulation :
  /////////////////////////////////////
  /// Full bisimulation experiments ///
  /////////////////////////////////////
  
  type R[A,B] = Set[(A,B)]
  type RC = R[Choreo,Local]
  type ROld = R[Choreo,Local]

  def findBisim(c:Choreo): RC =
    findBisim[Choreo,Local](Set(),Set((c,Local(c))))

  def findBisimPP(c:Choreo): String =
    findBisim(c).map(p=>s"${p._1}  ><  ${p._2} (${p._1.accepting} >< ${p._2.accepting})").mkString("\n")

  /** Find a bisimulation.
   *  Not checking yet if both sides are final or none is final in each member. */
  def findBisim[G:LTS,L:LTS](visited:R[G,L],
                             missing:R[G,L],i:Int=1): R[G,L] =
    //    println(s"[Sim] $visited  --  $missing")
    type S = R[G,L]
    missing.headOption match
      case Some((g:G,l:L)) =>
        if visited contains (g,l) then
          findBisim(visited,missing-((g,l)),i)
        else
          val nxtG = g.trans // next(cs._1)
          val nxtL = l.trans // next(cs._2)
          println(s"[Sim] Round $i @ $g -- doing ${(nxtG.map(_._1)++nxtL.map(_._1)).toSet.mkString(",")}")
//          if nxtL.isEmpty && !l.isEmpty then
//            println(s"[Sim] not a bisimulation:\n - $l is stuck but it is not done.")
//            return Set()

          // for every cs1 --a1-> cs1',
          //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
          var more: S = Set()

          // yin
          getMatches[G,L](nxtG,nxtL) match
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
            if (c.accepting != s.accepting) then // this always holds in our examples...
            println(s"[Sim] not a bisimulation:\n - $c ${
              if c.accepting then s"is accepting\n - $s is not"
              else s"is not accepting\n - $s is"}")
            // iterate
          findBisim(visited+((g,l)),(missing++more)-((g,l)),i+1)

      case None => visited // Success!


  /** for all steps in `steps1`, collect matching steps in `steps2`, and fail (Left) if any has no matches. */
  def getMatches[S1:LTS,S2:LTS](steps1:R[Action,S1],
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



////////////

  type RCW = R[GlobalTau,LocalTau]

  /** same as `findBisim`, but for weak bissimilarity and with tau-projections. */
  def findWBisim(c:Choreo): RCW =
    findWBisim[GlobalTau,LocalTau](Set(),Set((GlobalTau(c),LocalTau(c))))

  def findWBisimPP(c:Choreo): String =
    findWBisim(c).map(p=>s"${p._1}  ><  ${p._2} (${p._1.accepting} >< ${p._2.accepting})").mkString("\n")


  def findWBisim[G:LTS,L:LTS](visited:R[G,L],
                              missing:R[G,L],i:Int=1): R[G,L] =
    //    println(s"[Sim] $visited  --  $missing")
    type S = R[G,L]
    missing.headOption match
      case Some((g:G,l:L)) =>
        if visited contains (g,l) then
          findWBisim(visited,missing-((g,l)),i)
        else
          val nxtG = g.trans // next(cs._1)
          val nxtL = l.trans // next(cs._2)
          println(s"[Sim] Round $i @ $g -- doing ${(nxtG.map(_._1)++nxtL.map(_._1)).toSet.mkString(",")}")
          //          if nxtL.isEmpty && !l.isEmpty then
          //            println(s"[Sim] not a bisimulation:\n - $l is stuck but it is not done.")
          //            return Set()

          // for every cs1 --a1-> cs1',
          //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
          var more: S = Set()

          // yin
          getWMatches[G,L](nxtG,nxtL,l) match
            case Left(a) =>
              println(s"[Sim] not a bisimulation:\n - $g can do $a\n - $l cannot\n - tried $more")
              return Set()
            case Right(more1) => more = more ++ more1
          // yang
          getWMatches(nxtL,nxtG,g) match
            case Left(a) =>
              println(s"[Sim] not a bisimulation:\n - $l can do $a\n - $g cannot\n - tried $more")
              return Set()
            case Right(more2) => more = more ++ more2.map(_.swap)

          // check if newR has matching accepting states
          for (c,s)<-more do
            if (c.accepting != s.accepting) then // this always holds in our examples...
            println(s"[Sim] not a bisimulation:\n - $c ${
              if c.accepting then s"is accepting\n - $s is not"
              else s"is not accepting\n - $s is"}")
            // iterate
          findWBisim(visited+((g,l)),(missing++more)-((g,l)),i+1)
          
      case None => visited // Success!

  /** for all steps in `steps1`, collect matching steps in `steps2`, and fail (Left) if any has no matches. */
  def getWMatches[S1:LTS,S2:LTS](steps1:R[Action,S1],
                                 steps2:R[Action,S2], s2:S2): Either[Action,R[S1,S2]] =
    var more: R[S1,S2] = Set()
    for (a1,s1) <- steps1 do
      if a1==Tau then more += (s1,s2)
      else 
        val matches = getWMatches(a1,steps2)
        if matches.isEmpty then
        //println(s"[Sim] not a bisimulation:\n - $g can do $a2\n - $l cannot")
          return Left(a1) // fail! (no match)
        else
          more = more ++ matches.map((s1,_))
    Right(more)

  def getWMatches[S:LTS](a:Action,steps:R[Action,S]): Set[S] =
    val aMatches = for (a2,s2)<-steps // from all other steps
      if a2==a yield s2               // get the ones that do `a1` and collect the next state
    val tauMatches = for (a2,s2)<-steps if a2==Tau
      yield getWMatches(a,s2.trans)
    aMatches ++ tauMatches.flatten

