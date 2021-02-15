package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.Global
import choreo.choreo2.backend.Multiset._
import choreo.choreo2.backend.{LTS, Multiset}
import choreo.choreo2.analysis.{Local,LocalTau}


object Bisimulation :
  /////////////////////////
  /// Weak bisimulation ///
  /////////////////////////
  
  type R[A,B] = Set[(A,B)]
  type RC = R[Choreo,Local]
  type RC2 = R[GlobalTau,LocalTau]

  /** same as `findBisim`, but for weak bissimilarity and with tau-projections. */

  // FIXING bisimulations
  case class BEvid(msg:List[String],tried:Set[Int],count:Int)
  type BResult[A,B] = Either[BEvid,R[A,B]]
  
  // pretty print result
  def pp[A,B](res: BResult[A,B]): String = res match
    case Left(err) => "Not a bisim."+err._1.map("\n - "+_).mkString
    case Right(rel) => pp(rel) //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))
  def pp[A,B](rel:R[A,B]): String =
    val strs = rel.map((x,y)=>(x.toString.size,x.toString,y.toString)).toList.sorted.reverse
    val max = strs.map(_._1).max
    val strs2 = strs.map((_,x,y)=>(x+(" "*(max-x.size)),y))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}").mkString("\n")

  //// Variations of realizability checks ////

  def findBisim(c:Choreo): BResult[Choreo,Local] =
    findBisim[Choreo,Local](c,Local(c))

  def findBisimTau(c:Choreo): BResult[GlobalTau,LocalTau] =
    findBisim[GlobalTau,LocalTau](GlobalTau(c),LocalTau(c))
    
  //// Actual implementtion of branching bisimulation search ////
  
  /** Find a branching bisimulation. */
  def findBisim[G:LTS,L:LTS](g:G, l:L): BResult[G,L] =
    findWBisim2Aux(Set(),Set((g,l)),Set(),Nil,1)

  private def findWBisim2Aux[G:LTS,L:LTS](visited:R[G,L],
                                          missing:R[G,L],
                                          triedHash:Set[Int],
                                          lastError:List[String],
                                          i:Int): BResult[G,L] =
    // println(s"[Sim] $visited  --  $missing")
    type S = R[G,L]
    if i >= 5000 then
      return Left(BEvid(List("timeout",s"visited: $visited",s"missing: $missing"),triedHash,i)) 
    missing.headOption match
      // Success!
      case None => Right(visited) 
      
      // Already visited
      case Some((g,l)) if visited contains (g,l) =>
        findWBisim2Aux(visited,missing-((g,l)),triedHash,lastError,i)
        
      // Fail: not equally accepting
      case Some((g:G,l:L)) if g.accepting != l.accepting =>
        if g.accepting then 
          Left(BEvid(List(s"$g is accepting",s"$l is not"),triedHash,i)) else
          Left(BEvid(List(s"$l is accepting",s"$g is not"),triedHash,i))
        
      // traverse steps...
      case Some((g:G,l:L)) =>
        if i % 500 == 0 then
          println(s"[Sim] Round $i @ $g")// -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")
        
        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
        var more: Set[S] = Set(Set())
        def add(m:Set[S],g:G,l:L):Set[S] = m.map(_+((g,l))) 
        

        // for every g-a->g2
        for (a,g2)<- g.trans do
          if a==Tau then more = add(more,g2,l)
          else
            // exists l->a1->s2
            val tr = l.transW()
            val mbMatch = for (a2,l2,l3opt)<-tr if a==a2
                          yield l3opt match
                            // TODO: probably sometimes need to add 1 more pair to the bisim (pre-l2)
                            case None => add(more,g2,l2)
                            case Some(l3) => add(add(more,g2,l2),g2,l3) // also add post-tau transition
            if mbMatch.isEmpty then return Left(BEvid(List(s"$g can do $a",s"$l cannot"),triedHash,i))
            more = mbMatch.flatten

        // for every l-a->l2
        for (a,l2)<- l.trans do
          if a==Tau then more = add(more,g,l2)
          else
            // exists l->a1->s2
            val mbMatch = for (a2,g2,g3opt)<-g.transW() if a==a2
                          yield g3opt match
                            // TODO: probably sometimes need to add 1 more pair to the bisim (pre-l2)
                            case None => add(more,g2,l2)
                            case Some(g3) => add(add(more,g2,l2),g3,l2) // also add post-tau transition
            if mbMatch.isEmpty then return Left(BEvid(List(s"$g can do $a",s"$l cannot"),triedHash,i))
            more = mbMatch.flatten

  
        //// Collected all candidates to add to the bisimulation (`more`)
        //// Now we need to prune repeated steps, and try all options (collecting info when one branch fails).
        
        /// Avoiding recurrent paths...
        val newTry = (visited,more).hashCode
        if triedHash contains newTry then
          return Left(BEvid(lastError,triedHash,i))
                //findWBisim2Aux(visited,missing-((g,l)),triedHash,i+1)
  

        // check if, for any m<-more, a bisimulation can be found with `visited + m`
        var failed: Option[BEvid] = None
        var newTries = triedHash+newTry
        var newError = lastError
        var round = i

        if more.size>100 then
          println(s"[Sim] ($i - ${visited.hashCode} - ${more.hashCode}) options to visit: ${more.size}") //  \n"+more.map(_.hashCode).mkString("\n-----\n"))
  
        while (more.nonEmpty) do 
          val m = more.head
          findWBisim2Aux(visited + ((g, l)), missing++m, newTries, newError, round+1) match
            case Right(value) => return Right(value)
            case Left(err) =>
              failed = Some(err)
              round = err.count
              newTries ++= err.tried
              newError = err.msg
              more -= m

        failed match
          case Some(err) => Left(err) 
          case None => Right(visited)

