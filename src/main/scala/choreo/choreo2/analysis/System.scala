package choreo.choreo2.analysis

import choreo.choreo2.analysis.SOS._
import choreo.choreo2.backend.Multiset
import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo._


object System :
  type AMultiset = Multiset[Action]
  type System = (Set[Choreo],AMultiset)
  
  def pp(s:System): String =
    s._1.mkString("\n") +
      "---" +
      s._2.toString

  def initSys(c:Choreo): System =
    (allProj(c).values.toSet,Multiset())
  
  def isFinal(s:System): Boolean =
    s._2.isEmpty && s._1.forall(c => canSkip(c))
  
  def nextSys(c:Choreo): Set[(Action,System)] =
    next(initSys(c))

  def nextSysPP(s:System): Unit =
    actionsPP(next(s))

  def nextSysPP(c:Choreo): Unit =
    actionsPP(nextSys(c))
    
  def actionsPP(s:Set[(Action,System)]): Unit =
    for (a,s) <- s do
      println(s"$a: \n${
        s._1.map(" - "+_.toString).mkString("\n") +
        "\n ---\n " +
        s._2.toString
      }")
  
  def next(s:System): Set[(Action,System)] =
    val x = for (proj <- s._1) yield  // get each projection
    //println(s"next proj in sys is $proj")
      val proj2 = evolveProj(proj,s._2) // get all evolutions = (act,newProj,newNet)
      //println(s" - got evolution: $proj2")
      val newProj = for ((act,p2,n2)<-proj2) yield
        (act, (s._1-proj+p2 , n2) )
      //println(s" - updated evolution: $newProj")
      newProj.toSet
    x.flatten
  
  def evolveProj(c:Choreo, net:AMultiset): List[(Action,Choreo,AMultiset)] =
    for (act,chor)<-nextChoreo(c) if allowed(act,net) yield
      (act,chor, act match {
        case In(a,b,m)  => net - Out(b,a,m)
        case Out(a,b,m) => net + Out(a,b,m)
        case Tau => net 
      })
  
  def allowed(act: Action, net: AMultiset): Boolean =
    act match {
      case In(a, b, m) => net contains Out(b,a,m)
      case Out(_, _, _) => true
      case Tau => true
    }