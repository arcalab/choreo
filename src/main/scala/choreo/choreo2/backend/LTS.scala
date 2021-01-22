package choreo.choreo2.backend

//import choreo.choreo2.analysis.SOS.{canSkip, nextChoreo}
//import choreo.choreo2.analysis.Local.{System, initSys, isFinal, nextSys}
import choreo.choreo2.syntax.Choreo
import choreo.choreo2.syntax.Choreo.Action

trait LTS[S<:Any]:
  type St = S
  def trans: Set[(Action,LTS[S])]
  def accepting: Boolean
  def get:S
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