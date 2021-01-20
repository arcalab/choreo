package choreo.choreo2.analysis

import choreo.choreo2.analysis.Global
import choreo.choreo2.backend.{LTS, Multiset}
import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo._

case class Local(proj:Set[Choreo],netw:Multiset[Action]) extends LTS[Local] :
  
  def get:Local = this

  override def trans: Set[(Action,LTS[Local])] =
    Local.nextSys(proj,netw)

//  def pp: String =
//    proj.mkString("\n") +
//      "---" +
//      netw.toString

  override def toString: String =
    s"Local [${proj.mkString("] [")}] <${netw}>"
  
  override def accepting: Boolean =
    netw.isEmpty && proj.forall(c => Global(c).accepting)
  
    

object Local:
  type ABag = Multiset[Action]
  type System = (Set[Choreo],ABag)

  def apply(c:Choreo): Local =
    Local(allProj(c).values.toSet,Multiset())

  def nextSys(proj:Set[Choreo], netw:Multiset[Action]): Set[(Action,LTS[Local])] =
    //    var ends = List[AMultiset]()
    val x = for (p <- proj) yield  // get each projection
      //println(s"next proj in sys is $proj")
      val proj2 = evolveProj(p,netw) // get all evolutions = (act,newProj,newNet)
      //      val (proj2Tau,proj2Lbl) = proj2.partition(_._1==Tau) // split tau from non-tau evolutions
      //      ends = ends ++ proj2Tau.map(_._3)
      //println(s" - got evolution: $proj2")
      val newProj = for (act,p2,n2)<-proj2 yield
        (act, Local(proj-p+p2 , n2) )
        //println(s" - updated evolution: $newProj")
      newProj
    //    if ends.size == s._1.size // if all proj can skip
    //    then x.flatten + ((Tau,(Set(End),Multiset())))
    //    else 
    x.flatten


  def evolveProj(c:Choreo, net:ABag): Set[(Action,Choreo,ABag)] =
    for (act,chor)<-Global(c).trans if allowed(act,net) yield
  (act,chor.get.c, act match {
    case In(a,b,m)  => net - Out(b,a,m)
    case Out(a,b,m) => net + Out(a,b,m)
    case Tau => net
  })

  def allowed(act: Action, net: ABag): Boolean =
    act match
      case In(a, b, m) => net contains Out(b,a,m)
      case Out(_, _, _) => true
      case Tau => true

  
  def nextSys(c:Choreo): Set[(Action,LTS[Local])] =
    apply(c).trans

  def nextSysPP(s:Local): Unit =
    actionsPP(s.trans)

  def nextSysPP(c:Choreo): Unit =
    actionsPP(nextSys(c))

  def actionsPP(s:Set[(Action,LTS[Local])]): Unit =
    for (a,l) <- s do
    println(s"$a: \n${
      l.get.proj.map(" - "+_.toString).mkString("\n") +
        "\n ---\n " +
        l.get.netw.toString
    }")

