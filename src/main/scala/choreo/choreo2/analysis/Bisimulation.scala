package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.Global
import choreo.choreo2.backend.Multiset._
import choreo.choreo2.backend.{LTS, Multiset}
import choreo.choreo2.analysis.{LocalBasic,LocalManyTaus}


object Bisimulation :
  //////////////////////////////
  /// Branching bisimulation ///
  //////////////////////////////
  
  type R[A,B] = Set[(A,B)]
  type RT[A,B] = Map[(A,B),List[Action]] // relation with traces used to get there.
//  type RC = R[Choreo,LocalBasic]
//  type RC2 = R[GlobalTau,LocalManyTaus]

  /** Evidences that something that went wrong when searching for a bisimulation. */
  case class BEvid(msg:List[String],tried:Set[Int],count:Int)
  /** Result from a bisimulation search: either some evidence of what went wrong, or a bisimulation */
  type BResult[A,B] = Either[BEvid,RT[A,B]]
  
  /** Pretty printing bisimulation results. */
  def pp[A,B](res: BResult[A,B]): String = res match
    case Left(err) => "Not a bisim."+err._1.map("\n - "+_).mkString
    case Right(rel) => pp(rel) //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  /** Pretty printing bisimulations. */
  def pp[A,B](rel:RT[A,B]): String =
    val strs = rel.toList.map((xy,t)=>
          (xy._1.toString.size,xy._1.toString,xy._2.toString,t.mkString(",")))
          .toList.sorted.reverse
    val max = strs.map(_._1).max
    val strs2 = strs.map((_,x,y,t)=>(x+(" "*(max-x.size)),y,t))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}  @ ${p._3}").mkString("\n")

  //// Variations of realizability checks ////

  def findBisimBasic(c:Choreo): BResult[GlobalBasic,LocalBasic] =
    findBisim[GlobalBasic,LocalBasic](GlobalBasic(c),LocalBasic(c))

  def findBisimManyTaus(c:Choreo): BResult[GlobalManyTaus,LocalManyTaus] =
    findBisim[GlobalManyTaus,LocalManyTaus](GlobalManyTaus(c),LocalManyTaus(c))

  def findBisim(c:Choreo): BResult[Choreo,Local] =
    findBisim[Choreo,Local](c,Local(c))
  
  //// Actual implementtion of branching bisimulation search ////
  
  /** Find a branching bisimulation. */
  def findBisim[G:LTS,L:LTS](g:G, l:L): BResult[G,L] =
    findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),Nil,1)

  private def findWBisim2Aux[G:LTS,L:LTS](visited:RT[G,L],
                                          missing:RT[G,L],
                                          triedHash:Set[Int],
                                          lastError:List[String],
                                          i:Int): BResult[G,L] =
    // println(s"[Sim] $visited  --  $missing")
    type S = RT[G,L]
    if i >= 5000 then
      return Left(BEvid(List("timeout",s"visited: $visited",s"missing: $missing"),triedHash,i)) 
    missing.headOption match
      // Success!
      case None => Right(visited) 
      
      // Already visited
      case Some(ab,t) if visited contains ab =>
        findWBisim2Aux(visited,missing-ab,triedHash,lastError,i)
        
      // Fail: not equally accepting
      case Some(((g:G,l:L),t)) if g.accepting != l.accepting =>
        if g.accepting then 
          Left(BEvid(List(s"$g is accepting",s"$l is not",s"after ${t.reverse.mkString(",")}"),triedHash,i)) else
          Left(BEvid(List(s"$l is accepting",s"$g is not",s"after ${t.reverse.mkString(",")}"),triedHash,i))
        
      // traverse steps...
      case Some(((g:G,l:L),t)) =>
//        if i % 500 == 0 then
//          println(s"[Sim] Round $i @ $g")// -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")
        
        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
        var more: Set[S] = Set(Map())
        def add(m:Set[S],g:G,l:L,t:List[Action]):Set[S] = m.map(_+((g,l)->t)) 
        

        // for every g-a->g2
        for (a,g2)<- g.trans do
//          println(s"\n### doing $a\n[Sim] G $g ")
          if a==Tau then more = add(more,g2,l,Tau::t)
          else
            // exists l->a1->s2
            val tr = l.transW()
            val mbMatch = for (a2,l2,l3opt)<-tr if a==a2
                          yield l3opt match
                            // TODO: probably sometimes need to add 1 more pair to the bisim (pre-l2)
                            case None => add(more,g2,l2,a::t)
                            case Some(l3) => add(add(more,g,l3,Tau::t),g2,l2,a::Tau::t) // also add post-tau transition
            if mbMatch.isEmpty then
//              println(s"[Sim] L $l FAILS")
              return Left(BEvid(List(s"$g can do $a",s"$l cannot do τ*,$a",s"after ${t.reverse.mkString(",")}"),triedHash,i))
//            println(s"[Sim] L $l matches") // by:\n - ${tr.filter(_._1==a).map(x=>s"${x._2} [${x._3}]").mkString("\n - ")}")
//            println(s"[Sim] Adding to the bisim:\n + $g2\n - ${tr.filter(_._1==a).map(x=>s"${x._2} {tau: ${x._3}}").mkString("\n - ")}")
            more = mbMatch.flatten

        // for every l-a->l2
        for (a,l2)<- l.trans do
//          println(s"\n### doing $a\n[Sim] L $l ")
          if a==Tau then
            more = add(more,g,l2,Tau::t)
//            println(s"[Sim] adding $g <--> $l2")
          else
            // exists l->a1->s2
            val tr = g.transW()
            val mbMatch = for (a2,g2,g3opt)<-tr if a==a2
                          yield g3opt match
                            // TODO: probably sometimes need to add 1 more pair to the bisim (pre-l2)
                            case None => add(more,g2,l2,a::t)
                            case Some(g3) => add(add(more,g3,l,Tau::t),g2,l2,a::Tau::t) // also add post-tau transition
            if mbMatch.isEmpty then
//              println(s"[Sim] G $l FAILS")
              return Left(BEvid(List(s"$l can do $a",s"$g cannot do τ*,$a",s"after ${t.reverse.mkString(",")}"),triedHash,i))
//            println(s"[Sim] G $g matches.") // by:\n - ${tr.filter(_._1==a).map(x=>s"${x._2} [${x._3}]").mkString("\n - ")}")
//            println(s"[Sim] Adding to the bisim:\n + $l2\n - ${tr.filter(_._1==a).map(x=>s"${x._2} {tau: ${x._3}}").mkString("\n - ")}")
            more = mbMatch.flatten

//        println("---")

  
        //// Collected all candidates to add to the bisimulation (`more`)
        //// Now we need to prune repeated steps, and try all options (collecting info when one branch fails).
        
        /// Avoiding recurrent paths...
        val newTry = (visited.keys,more.map(_.keys)).hashCode
        if triedHash contains newTry then
          return Left(BEvid(lastError,triedHash,i))
                //findWBisim2Aux(visited,missing-((g,l)),triedHash,i+1)
  

        // check if, for any m<-more, a bisimulation can be found with `visited + m`
        var failed: Option[BEvid] = None
        var newTries = triedHash+newTry
        var newError = lastError
        var round = i

//        if more.size>100 then
//          println(s"[Sim] ($i - ${visited.hashCode} - ${more.hashCode}) options to visit: ${more.size}") //  \n"+more.map(_.hashCode).mkString("\n-----\n"))
  
        while (more.nonEmpty) do 
          val m = more.head
          findWBisim2Aux(visited + ((g,l)->t), missing++m, newTries, newError, round+1) match
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

