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
  def findBisim2[G:LTS,L:LTS](g:G,l:L): R[G,L] =
    findBisim[G,L](Set(),Set((g,l)))

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
            case Right(more1) => more = more ++ more1 // TODO: fix implementation (now, states "for every c1-a->c1', then for EVERY c2-a->c2'..."  
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


///////////////////////////////////////////////////////////////////////////

  // FIXING bisimulations
  private type BEvidence = (List[String],Int)
  def findWBisim2(c:Choreo): Either[BEvidence,R[Choreo,Local]] =
    findWBisim2[Choreo,Local](c,Local(c))
    
  def findWBisim2PP(c:Choreo): Unit =
    findWBisim2(c) match
      case Left(err) => println("Not a bisim."+err._1.map("\n - "+_).mkString)
      case Right(rel) => println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  def findWBisim2[G:LTS,L:LTS](g:G,l:L): Either[BEvidence,R[G,L]] =
    findWBisim2Aux(Set(),Set((g,l)),1)

  private def findWBisim2Aux[G:LTS,L:LTS](visited:R[G,L],
                                          missing:R[G,L],i:Int): Either[BEvidence,R[G,L]] =
    //    println(s"[Sim] $visited  --  $missing")
    type S = R[G,L]
    if i >= 20000 then
      return Left(List("timeout",s"visited: $visited",s"missing: $missing"),i) 
    missing.headOption match
      // Success!
      case None => Right(visited) 
      
      // Already visited
      case Some((g,l)) if visited contains (g,l) =>
        findWBisim2Aux(visited,missing-((g,l)),i)
        
      // Fail: not equally accepting
      case Some((g:G,l:L)) if g.accepting != l.accepting =>
        if g.accepting then 
          Left(List(s"$g is accepting",s"$l is not"),i) else
          Left(List(s"$l is accepting",s"$g is not"),i)
//        printf(s"[Sim] Not a bisimulation:\n - ${err.mkString("\n - ")}")
        
      // traverse steps...
      case Some((g:G,l:L)) =>
        //println(s"[Sim] Round $i @ $g -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")
        
        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
        var more: Set[S] = Set(Set())
        def add(m:Set[S],g:G,l:L):Set[S] = m.map(_+((g,l))) 
          

        // for every g-a->g2
        for (a,g2)<- g.trans do
          if a==Tau then more = add(more,g2,l)
          else
            // exists l->a1->s2
            val mbMatch = for (a2,l2)<-l.transW if a==a2 yield add(more,g2,l2)
            if mbMatch.isEmpty then return Left(List(s"$g can do $a",s"$l cannot"),i)
            more = mbMatch.flatten

        // for every l-a->l2
        for (a,l2)<-l.trans do
          if a==Tau then more = add(more,g,l2)
          else
            // exists g->a1->g2
            val mbMatch = for (a2,g2)<-g.transW if a==a2 yield add(more,g2,l2)
            if mbMatch.isEmpty then return Left(List(s"$l can do $a",s"$g cannot"),i)
            more = mbMatch.flatten
        
        // check if, for any m<-more, a bisimulation can be found with m
        var failed: Option[BEvidence] = None
        var round = i
        //println(s"[Sim] ($i) options to visit: ${more.size}, visited: ${visited.size}")
        while (more.nonEmpty) do 
          val m = more.head
          findWBisim2Aux(visited + ((g, l)), missing++m, round+1) match
            case Right(value) => return Right(value)
            case Left((err,i2)) =>
              failed = Some((err,i2))
              round = i2
              more -= m

        failed match
          case Some(err) => Left(err) 
          case None => Right(visited)
