package choreo.analysis

import choreo.syntax.{Agent, Choreo, Msg}
import Choreo.*
import caos.sos.SOS
import choreo.projection.ChorNoTauProj
import choreo.sos.ChorDefSOS

/**
 * A connector is well-branched if, forall subconnector C = C1 + C2:
 * - exists a!=b, x!=y
 *	 - C1_a - ab!x is the unique min (single "next")
 *	 - C2_a - ab!y is the unique min
 *	 - C1_b - ab?x is the unique min
 *	 - C2_b - ab?y is the unique min
 *- forall d notin {a,b}
 *	 - C1_d = C2_d OR
 *X	 - exists z!=w
 *X		 - C1_d - ad!z
 *X		 - C2_d - ad!w
 */
object WellBranched:

  type Result = Either[String,Unit] // either Left error, or Right ()
  extension (r:Result)
    def toBool = r.isRight
    def &&(other:Result): Result = (r,other) match
      case (Left(e1),Left(e2)) => Left(s"$e1; $e2")
      case (Left(e),_) => Left(e)
      case (_,Left(e)) => Left(e)
      case _ => Right(())
    def show: String = r match
      case Left(e) => s"Error: $e"
      case Right(_) => "OK"

  def showResult(r:Result) = r.show

  def apply(c:Choreo)(using sos:SOS[Action,Choreo] = ChorDefSOS): Result = c match
    case Choice(c1, c2) => wellBranched(c1,c2)
    //
    case Seq(c1, c2) => apply(c1) && apply(c2)
    case Par(c1, c2) => apply(c1) && apply(c2)
    case DChoice(c1, c2) => apply(Choice(c1,c2))
    case Loop(c) => apply(c)
    case _ => Right(())

  def wellBranched(c1: Choreo, c2: Choreo)(using sos:SOS[Action,Choreo]): Result =
    def getNextProj(c:Choreo): Set[(Agent,Choreo,Set[Action])] =
      ChorNoTauProj.allAProj(c).map((a, c1a)=> (a,c1a, sos.next(c1a).map(_._1)))
    val nexts1 = getNextProj(c1)
    val nexts2 = getNextProj(c2)
    // collects all single ab!x in c1 with a matching ab!y in c2


    // collects all single "ab!?x" in c1 with matching "ab?!x" in c2
    def getActionElm(action: Choreo.Action) = action match
      case In(a,b,m) => List((b,a,m))
      case Out(a,b,m) => List((a,b,m))
      case _ => Nil

    def matchSent(a1:Action,a2:Action): Option[(Agent,Agent,Msg,Msg)] = (a1,a2) match
      case (Out(a1,b1,m1),Out(a2,b2,m2)) if a1==a2 && b1==b2 => Some((a1,b1,m1,m2))
      case _ => None
    // set of ab!x and ab!y matching actions from 2 options, returning (a,b,x,y)
    def findMatch(a:Action, as: Set[Action]): List[(Agent,Agent,Msg,Msg)] =
      as.toList.map(a2 => matchSent(a,a2).toList).flatten
    // returns all matches after traversing nexts of C1
    val matches: Set[List[(Agent,Agent,Msg,Msg)]] =
      for (a,c1a,nxt1)<-nexts1 if nxt1.size == 1 && nxt1.head.isOut yield
        findMatch(nxt1.head,nexts2.flatMap(_._3))
    val matchesf = matches.flatten
    // checks if there is only 1 such a->b:x
    if matchesf.isEmpty then return Left(s"No pair of leading sends found. [${
      nexts1.flatMap(_._3).mkString(",")}] + [${nexts2.flatMap(_._3).mkString(",")}]")
    if matchesf.size != 1 then return Left(s"Found multiple (or none) leading messages: [${
      matchesf.map(x=>s"${x._1}->${x._2}:${x._3}").mkString(",")}]")
    // checks if all c1 and c2 projections are either for a->b:x or the same as a neighbour
    val (a,b,m1,m2):(Agent,Agent,Msg,Msg) = matchesf.head
    def unmatched(c:Choreo,cs:Set[Choreo]):Boolean = !cs.contains(c)
    for (ag,cag,nxt) <- nexts1 do
      if nxt!=Set(In(b,a,m1)) && nxt!=Set(Out(a,b,m1)) &&
         nxt!=Set(In(b,a,m2)) && nxt!=Set(Out(a,b,m2)) &&
         unmatched(cag,nexts2.map(_._2))
      then return Left(s"Agent $ag can do ${nxt.mkString(",")} in ${c1} and is not matched in $c2")
    for (ag,cag,nxt) <- nexts2 do
      if nxt!=Set(In(b,a,m1)) && nxt!=Set(Out(a,b,m1)) &&
        nxt!=Set(In(b,a,m2)) && nxt!=Set(Out(a,b,m2)) &&
        unmatched(cag,nexts1.map(_._2))
      then return Left(s"Agent $ag can do ${nxt.mkString(",")} in ${c1} and is not matched in $c2")
    // No error found. All is good.
    Right(())




