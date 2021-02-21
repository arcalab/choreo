package choreo.choreo2.backend

//import choreo.choreo2.analysis.SOS.{canSkip, nextChoreo}
//import choreo.choreo2.analysis.Local.{System, initSys, isFinal, nextSys}
import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo.{Action, In, Out, Tau}

//trait LTS[S<:Any]:
//  type St = S
//  def trans: Set[(Action,LTS[S])]
//  def accepting: Boolean
//  def get:S
//  
//  // auxiliary functions
//  def transPP: String = trans
//    .map(p=>s"${p._1} ~~> ${p._2.get}")
//    .mkString("\n")
//  def steps(n:Int): Set[(List[Action],Option[S])] = n match
//    case 0 => Set(Nil -> Some(get))
//    case _ =>
//      val nc = trans
//      nc.flatMap(p=> {
//        val rec = p._2.steps(n-2)
//        if rec.isEmpty then
//          List(List(p._1) -> None)
//        else
//          for s <- rec
//            yield (p._1::s._1) -> s._2
//      })


trait LTS[S<:Any]:
  type St = S
  extension (s:S)
    def trans: Set[(Action,S)]
    def accepting: Boolean
    //def get:S

    // auxiliary functions
    /** All (a,s'') such that: s -tau->* s' -a-> s''  */
    def transW(last:Option[S]=None): Set[(In | Out,S,Option[S])] =
      (for (a,s2)<-s.trans yield
        a match 
          case Tau => s2.transW(Some(s2))
          case x:(In|Out) => Set((x,s2,last)) 
      ).flatten
    
    def taus: Set[S] =
      (for (a,s2)<-s.trans yield
        a match
          case Tau => s2.taus + s
          case x => Set(s))
      .flatten + s
    
    //  def transBy(a:Action): LTS[S] 
    def transPP: String = s.trans
      .map(p=>s"${p._1} ~~> ${p._2}")
      .mkString("\n")
    
    def steps(n:Int): Set[(List[Action],Option[S])] = n match
      case 0 => Set(Nil -> Some(s))
      case _ =>
        val nc = s.trans
        nc.flatMap(p=> {
          val rec = p._2.steps(n-1)
          if rec.isEmpty then
            List(List(p._1) -> None)
          else
            for s <- rec
              yield (p._1::s._1) -> s._2
        })

    def getAllStates: Set[S] = 
      var done:Set[S] = Set(s)
      var next = (for (a,s2) <- s.trans yield s2) + s
      while next.nonEmpty do
        val x = next.head
        next -= x
        val next2 = (for (_,x2) <- x.trans yield x2) -- done
        done ++= next2
        next ++= next2
      done
        


//  def isEmpty: Boolean


//case class Global(c:Choreo) extends LTS[Choreo]:
//  def trans: Set[(Action,LTS[Choreo])] =
//    for (a,c2) <- nextChoreo(c).toSet yield (a,Global(c2))
//  def accepting: Boolean = canSkip(c)
////  def isEmpty: Boolean = c==End // never stuck
//  def get:Choreo = c
//  override def toString: String =
//    s"Global [$c]"

//case class Local2(s:System) extends LTS[System](s):
//  def trans: Set[(Action,LTS[System])] =
//    for (a,s2) <- nextSys(s:System) yield (a,Local2(s2))
//  def accepting:Boolean = isFinal(s)
////  def isEmpty: Boolean =
////    s._2.isEmpty && s._1.forall(c => c==End)
//  override def toString: String =
//    s"Local [${s._1.mkString("] [")}] <${s._2}>"
//
//object Local2:
//  def apply(c:Choreo): Local2 =
//    Local2(initSys((c)))