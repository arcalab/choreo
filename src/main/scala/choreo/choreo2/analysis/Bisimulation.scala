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
  case class BEvid(msgs:Set[List[String]],tried:Set[Int],count:Int)
  /** Result from a bisimulation search: either some evidence of what went wrong, or a bisimulation */
  type BResult[A,B] = Either[BEvid,RT[A,B]]
  
  /** Pretty printing bisimulation results. */
  def pp[A,B](res: BResult[A,B]): String = res match
    case Left(err) => "Not a bisim."+err.msgs.map(m => m.map("\n - "+_).mkString).mkString("\n---")
    case Right(rel) => pp(rel) //println(rel.map(p=>s"- ${p._1}   <->   ${p._2}").mkString("\n"))

  /** Pretty printing bisimulations. */
  def pp[A,B](rel:RT[A,B]): String =
    val strs = rel.toList.map((xy,t)=>
          (t.size,xy._1.toString,xy._2.toString,
            if t.isEmpty then "init" else t.mkString(",")))
          .toList.sorted
    val max = strs.map(_._2.size).max
    val strs2 = strs.map((_,x,y,t)=>(x+(" "*(max-x.size)),y,t))
    strs2.map(p=>s"- ${p._1}  <->  ${p._2}  @ ${p._3}").mkString("\n")

  //// Variations of realizability checks ////

  def findBisimBasic(c:Choreo): BResult[GlobalBasic,LocalBasic] =
    findBisim[GlobalBasic,LocalBasic](GlobalBasic(c),LocalBasic(c))

  def findBisimManyTaus(c:Choreo): BResult[GlobalManyTaus,LocalManyTaus] =
    findBisim[GlobalManyTaus,LocalManyTaus](GlobalManyTaus(c),LocalManyTaus(c))

  def findBisim(c:Choreo): BResult[Choreo,Local] = {
    if Bounded.boundedChoreo(c)
    then  findBisim[Choreo,Local](c,Local(c))
    else Left(BEvid(Set(List("Found an unbounded loop.")),Set(),0))
    
  }

  //// Actual implementtion of branching bisimulation search ////
  
  /** Find a branching bisimulation. */
  def findBisim[G:LTS,L:LTS](g:G, l:L): BResult[G,L] =
    findWBisim2Aux(Map(),Map((g,l)->Nil),Set(),1)

  private def findWBisim2Aux[G:LTS,L:LTS](visited:RT[G,L],
                                          missing:RT[G,L],
                                          triedHash:Set[Int], // to avoid redundant searches
//                                          lastError:List[String],
                                          i:Int) // to count how many runs
                                         : BResult[G,L] =
//    println(s"[Sim] $visited  --  $missing")
    type S = RT[G,L]
    if i >= 5000 then
      return Left(BEvid(Set(List("timeout",s"visited: $visited",s"missing: $missing")),triedHash,i))
    missing.headOption match
      // Success!
      case None => Right(visited) 
      
      // Already visited
      case Some(ab,t) if visited contains ab =>
        findWBisim2Aux(visited,missing-ab,triedHash,i)
        
      // Fail: not equally accepting
      case Some(((g:G,l:L),t)) if g.accepting != l.accepting =>
        if g.accepting then 
          Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",s"$g is accepting",s"$l is not")),triedHash,i)) else
          Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",s"$l is accepting",s"$g is not")),triedHash,i))
        
      // traverse steps...
      case Some(((g:G,l:L),t)) =>
//        if i % 500 == 0 then
//        println(s"\n#####\n[Sim] Round $i @ $g")// -- doing ${(g.trans.map(_._1)++l.trans.map(_._1)).toSet.mkString(",")}")
        
        // for every cs1 --a1-> cs1',
        //   exists cs2 --a2--> cs2' & cs1'R cs2' [& cs1' fin = cs2' fin]
        var more: Set[S] = Set(Map())
        def one(g:G,l:L,t:List[Action]):Set[S] = Set(Map((g,l)->t))
        def add(m:Set[S],g:G,l:L,t:List[Action]):Set[S] = m.map(_+((g,l)->t))
        def and(x:Set[S],y:Set[S]): Set[S] = 
          for m1<-x; m2<-y yield m1++m2

        def or(x:Set[S],y:Set[S]): Set[S] =
          x ++ y
        def ors(x:Set[Set[S]]): Set[S] =
          x.flatten
        

        // for every g-a->g2
        for (a,g2)<- g.trans do
//          println(s"\n### doing $a\n[Sim] G $g ")
          if a==Tau then more = add(more,g2,l,Tau::t)
          else
            // exists l->a1->s2
            val tr = l.transW()
            ///// more = set([])
            val mbMatch = for (a2,l2,l3opt)<-tr if a==a2
                          yield l3opt match
                            case None => one(g2,l2,a::t)
                            case Some(l3) => and( one(g,l3,Tau::t) , one(g2,l2,a::Tau::t) )
            if mbMatch.isEmpty then
//              println(s"[Sim] L $l FAILS")
              return Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",
                                         s"$g can do $a",
                                         s"$l cannot do τ*,$a")),triedHash,i))
//            println(s"[Sim] L $l matches") // by:\n - ${tr.filter(_._1==a).map(x=>s"${x._2} [${x._3}]").mkString("\n - ")}")
//            println(s"[Sim] Adding to the bisim:\n + $g2\n - ${tr.filter(_._1==a).map(x=>s"${x._2} {tau: ${x._3}}").mkString("\n - ")}")
            more = and(more , ors(mbMatch)) //mbMatch.flatten
//            println(s"[Sim] (Global 'more'): ${more.size}\n"+more.map(
//                m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )").mkString("\n")).mkString("\n-----\n"))
//            println(s"[B] new mbMatch: $mbMatch")

        val moreRight = more
        more = Set(Map())
  
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
                            case None => one(g2,l2,a::t)
                            case Some(g3) => and( one(g3,l,Tau::t), one(g2,l2,a::Tau::t) ) // also add post-tau transition
            if mbMatch.isEmpty then
//              println(s"[Sim] G $l FAILS")
              return Left(BEvid(Set(List(s"after ${if t.isEmpty then "[]" else t.reverse.mkString(",")}",
                                         s"$l can do $a",
                                         s"$g cannot do τ*,$a"
                                        )),triedHash,i))
//            println(s"[Sim] G $g matches.") // by:\n - ${tr.filter(_._1==a).map(x=>s"${x._2} [${x._3}]").mkString("\n - ")}")
//            println(s"[Sim] Adding to the bisim:\n + $l2\n - ${tr.filter(_._1==a).map(x=>s"${x._2} {tau: ${x._3}}").mkString("\n - ")}")
            more = and(more , ors(mbMatch)) //mbMatch.flatten
//            println(s"[Sim] (Local 'more'): ${more.size}\n"+more.map(
//                m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )").mkString("\n")).mkString("\n-----\n"))

//        println("---")
        
        val moreLeft = more
        more = and(moreLeft , moreRight)
  
//        if more.isEmpty
//        then return Left(BEvid(Set(List("No agreement on what to do",
//          s"left: $g",
//          s"right: $l",
//          s"Left: ${moreLeft.map(
//                        m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )")
//                          .mkString("\n"))
//                  .mkString("\n-----\n")}",
//          s"Right: ${moreRight.map(
//            m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )")
//              .mkString("\n"))
//            .mkString("\n-----\n")}"
//          )),triedHash,i)) 
  
        //// Collected all candidates to add to the bisimulation (`more`)
        //// Now we need to prune repeated steps, and try all options (collecting info when one branch fails).
        
        /// Avoiding recurrent paths...
        val newTry = (visited.keys,missing,more.map(_.keys)).hashCode
        if triedHash contains newTry then
//          println(s"[Sim] Tried $newTry -> FAIL")
          return Left(BEvid(Set(),triedHash,i))
                // Left(BEvid(lastError,triedHash,i))
                //findWBisim2Aux(visited,missing-((g,l)),triedHash,i+1)


        // check if, for any m<-more, a bisimulation can be found with `visited + m`
        var failed: Option[BEvid] = None
        var newTries = triedHash+newTry
//        var newError = lastError
        var round = i

//        if more.size>100 then
//            println(s"[Sim] ($i - ${visited.hashCode} - ${more.hashCode}) options to visit: ${more.size}") //  \n"+more.map(_.hashCode).mkString("\n-----\n"))
//        println(s"[Sim] ($i - $newTry) options to visit: ${more.size}\n"+more.map(
//            m => m.toList.map(x=>s" - ${x._2} -> ${x._1._1} <-> ${x._1._2} )").mkString("\n")).mkString("\n-----\n"))
//        var go=1
  
        while (more.nonEmpty) do 
          val m = more.head // next option to try
//          println(s"[Sim] NEXT: $go/$newTry/$newTries/${m.keys.mkString(", ")}/${missing.keys.mkString(", ")}")
          findWBisim2Aux(visited + ((g,l)->t), missing++m, newTries, round+1) match
            case Right(value) =>
//              println(s"[Sim] YES: $go/$newTry/${m.keys.mkString(", ")}")
              return Right(value)
            case Left(err) =>
//              println(s"[Sim] nope: $go/$newTry/${m.keys.mkString(", ")}")
              newTries ++= err.tried
              round = err.count
//              go += 1
              failed = failed match
                case None => Some(err)
                case Some(BEvid(msgs,tried,count)) => Some(BEvid(msgs++err.msgs,newTries,round))
//              newError = err.msg
              more -= m

        failed match
          case Some(err) => Left(err) 
          case None => Right(visited)

