package choreo.sos

import choreo.common.Simplify
import choreo.syntax.Choreo.*
import choreo.syntax.*
import caos.sos.SOS
import caos.sos.SOS.*
import choreo.sos.ChorSyncSOS.Interact

import scala.annotation.tailrec
import scala.sys.error


/** Current attempt to give a semantics to Choreo, based on a delay-sequence `s1;s2`
 * that can adapt terms `s1` by commiting to choices that will enable `s2`.  */
object ChorSyncSOS extends SOS[Interact,Choreo]:
  case class Interact(from:Set[Agent],to:Set[Agent],m:Msg):
    def toSend = Send(from.toList,to.toList,m)

    /** Set of involved agents */
    def agents = from++to

    def filter(a:Agent) =
      Interact(from.filter(_==a),to.filter(_==a),m)

    override def toString: String =
      if from==to
      then s"${from.mkString(",")}${m.pp}"
      else s"${from.mkString(",")}->${to.mkString(",")}${m.pp}"

  override def next[A>:Interact](c:Choreo): Set[(A, Choreo)] = nextChoreo(c).toSet

  /* Not considering the accepting state for now. */
  override def accepting(c:Choreo): Boolean = false

  def canTerminate(c: Choreo): Boolean = c match
    case _:Send => false
    case Seq(c1, c2) => canTerminate(c1) && canTerminate(c2)
    case Par(c1, c2) => canTerminate(c1) && canTerminate(c2)
    case Choice(c1, c2) => acceptChoice(c1,c2) //accepting(c1) || accepting(c2)
    case DChoice(c1, c2) => acceptChoice(c1,c2) //accepting(c1) || accepting(c2)
    case Loop(_) => true
    case End => true
    case Tau => false // reverting to true (not sure what makes more sense...)
    case _: Action => false // NOT including tau

  private def acceptChoice(c1: Choreo, c2:Choreo): Boolean =
    (canTerminate(c1) || canTerminate(c2))
//    (accepting(c1) && noIns(c2))  ||
//    (accepting(c2) && noIns(c1))

//  private def noIns(c:Choreo): Boolean =
//    nextChoreo(c).forall(pair => !pair._1.isInstanceOf[In])

  // Adapt to avoid simplification
  private def simplify(c:Choreo):Choreo =
    //Simplify(c)
    c match
      case Seq(End,Loop(c2)) => Loop(c2)
      case _ => c

  /** SOS: next step of a Choreo expression */
  def nextChoreo[A>:Interact](c:Choreo): List[(A,Choreo)] = {
    //println(s"next of $c...")
    c match
      case s@Send(as, bs, m) =>
//        if (as:::bs).exists(ignore) then Nil else
        List(Interact(as.toSet,bs.toSet,m) -> End)
      case Seq(c1, c2) =>
        // println(s"seq: $c1 [;] $c2")
        val nc1 = nextChoreo(c1)
          //// nagrees is used for weak-sequencing, ignored here.
//        var nagrees:List[(Interact,Choreo)] = Nil
//        for ((l,c3) <- nextChoreo(c2))
//          val c1a = agrees(c1,l)
//          if c1a.nonEmpty then nagrees ++= c1a.map(p=> l->Simplify(p>c3))
        // --------------------------------------
        nc1.map(p=>p._1->simplify(p._2>c2)) ++ // do c1
//          nagrees // do c2 if c1 agrees with
          (if canTerminate(c1)
          then nextChoreo(c2).map((aa,cc)=>aa->simplify(cc))
          else List())
      case Par(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        nc1.map(p => p._1 -> simplify(p._2||c2)) ++
          nc2.map(p => p._1 -> simplify(c1||p._2))
      case Choice(c1, c2) =>
        val nc1 = nextChoreo(c1)
        val nc2 = nextChoreo(c2)
        ///// comment last part to hide the rule c1+c2 -tau-> 0
        nc1 ++ nc2 // ++ (if accepting(c1)||accepting(c2) then List(Tau -> End) else Nil)
//      case DChoice(c1,c2) => //todo: check
//        val nc1 = nextChoreo(c1)
//        val nc2 = nextChoreo(c2)
//        val ja = nc1.map(_._1).intersect(nc2.map(_._1))
//        val nj = ja.flatMap(a => group(a,nc1,nc2))
//        val ns = nc1.filterNot(a => ja.contains(a._1)) ++ nc2.filterNot(a=> ja.contains(a._1))
//        nj ++ ns
      case Loop(c2) =>
        val nc2 = nextChoreo(c2)
        nc2.map(p=>p._1 -> (p._2>c))
      case End => Nil
      case Tau => List(Interact(Set(),Set(),Msg(Nil)) -> End) // not reachable
      case In(a, b, m) =>
        List(Interact(Set(b),Set(a),m) -> End)
      case Out(a, b, m) =>
        List(Interact(Set(a),Set(b),m) -> End)
      case Internal(a, m) =>
        List(Interact(Set(a),Set(a),m) -> End)
      case _ => error(s"Unknown next for $c")
  }
  //nxt

  def group(a:Action,nc1:List[(Action,Choreo)],nc2:List[(Action,Choreo)]):List[(Action,Choreo)] =
    var joinedSteps:List[(Action,Choreo)] = List()
    for ((a1,c1) <- nc1; (a2,c2) <- nc2; if a1 == a && a2 == a) do
      joinedSteps +:= ((a1,c1+c2))
    joinedSteps


  // c' = agrees(c,a)  ==>  c --check a-> c'
  /** Checks partial termination, used for weak sequencing.
   * Current version is using only the strong sequencing, so this is not used. */
  def agrees(c:Choreo,a:Interact):Option[Choreo] = c match {
    case End => Some(c)
    case Loop(c1) =>
      val t = agrees(c1,a)
      if inNext(c1,t) then Some(c)
      else Some(End)
    case Seq(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if (t1.isEmpty || t2.isEmpty) then None
      else for (nc1<- t1 ; nc2<- t2)
              yield Seq(nc1,nc2)
    case Par(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if (t1.isEmpty || t2.isEmpty) then None
        else for (nc1<- t1; nc2<- t2)
                yield Par(nc1,nc2)
    case Choice(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if t1.isEmpty then t2
      else if t2.isEmpty then t1
      else for (nc1<- t1 ; nc2<- t2)
        yield Choice(nc1,nc2)
    case DChoice(c1,c2) =>
      val t1 = agrees(c1,a)
      val t2 = agrees(c2,a)
      if t1.isEmpty then t2
      else if t2.isEmpty then t1
      else for (nc1<- t1 ; nc2<- t2)
        yield DChoice(nc1,nc2)
    case Send(as, bs, m) =>
      if !(as:::bs).exists(agents(a.toSend)) then Some(c)
      else None
    case In(a1,_,m) =>
      if !(agents(a.toSend) contains a1) then Some(Send(List(a1),Nil,m))
      else None
    // only used for the evolution of projections
    case Out(a1,_,m) =>
      if !(agents(a.toSend) contains a1) then Some(Send(Nil,List(a1),m))
      else None
    case Internal(a1, m) =>
      if !(agents(a.toSend) contains a1) then Some(Send(List(a1),List(a1),m))
      else None
    case Tau =>
      Some(End) // todo: experiment now - drop taus if we do a follow up action.
      //Some(c)
//    case _ =>   error(s"Unknonwn agrees with $a for $c")
  }

  // checks if a choreo appears as a next step in a list of transitions
  private def inNext(c:Choreo,t:Option[Choreo]):Boolean =
    t contains c //t.find(s=>s==c).isDefined

  private def nextPP(c:Choreo): String =
    SOS.nextPP(ChorDefSOS,c) // Global(c).transPP

//  private def nextSPP(c:Choreo, n:Int): String =
//    goS(c,n).mkString("\n")
//
//  /** Older version, to be replaced by nextS. */
//  private def goS(c:Choreo,n:Int): Set[String] = n match
//    case 0 => Set(s"~~> $c")
//    case _ =>
//      val nc = ChorDefSOS.next(c) //Global(c).trans
//      nc.flatMap(p=> {
//        val rec = goS(/*p._2.get.c*/ p._2,n-1)
//        if rec.isEmpty then
//          List(s"${p._1} [Done]")
//        else {
//          var fst = true
//          val indent = " ".repeat(p._1.toString.length)+"   "
//          for s <- rec
//            yield s"${if fst then {fst=false; p._1.toString+" \\ "} else indent}$s"
//        }
//      })




      
