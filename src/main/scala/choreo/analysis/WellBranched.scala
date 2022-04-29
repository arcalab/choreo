package choreo.analysis

import choreo.syntax.{Agent, Choreo, Msg}
import Choreo.*
import caos.sos.SOS
import choreo.projection.ChorNoTauProj
import choreo.sos.ChorDefSOS

/**
 * A connector is well-branched if, forall subconnector C = C1 + C2:
 * - exists a!=b, X!=Y
 *   - [new] a is the ONLY leader of c1 and c2
 *	 - C1_a - ab!x is the unique min (single "next") -- correction: all min of C1 in "a" have shape ab!x, forall x in X
 *	 - C2_a - ab!y is the unique min -- correction: all min of C2 in "a" have shape ab!y, forall y in Y
 *	 - C1_b - ab?x is the unique min -- correction: all min of C1 in "b" have shape ab?x, forall x in X (I think)
 *	 - C2_b - ab?y is the unique min -- correction: all min of C2 in "b" have shape ab?y, forall y in Y (I think)
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
    case Choice(c1, c2) => wellBranched(c1,c2) && apply(c1) && apply(c2)
    //
    case Seq(c1, c2) => apply(c1) && apply(c2)
    case Par(c1, c2) => apply(c1) && apply(c2)
    case DChoice(c1, c2) => apply(Choice(c1,c2))
    case Loop(c) => apply(c)
    case _ => Right(())

  /** Core of the well-brancheness analysis. Given two expressions (choices),
   * check if they obey the well-branchness conditions. */
  // deprecated, since this version had problems.
  @deprecated
  def wellBranchedOld(c1: Choreo, c2: Choreo)(using sos:SOS[Action,Choreo]): Result =
    println(s"--- $c1 OR $c2 ---")
    def getNextProj(c:Choreo): Set[(Agent,Choreo,Set[Action])] =
      ChorNoTauProj.allAProj(c).map((a, c1a)=> (a,c1a, sos.next(c1a).map(_._1)))
    val nexts1 = getNextProj(c1)
    val nexts2 = getNextProj(c2)

    //// Check 1: only one leader!
    val leaders1: Set[Agent] =  for (ag,_,nxt)<-nexts1; act<-nxt if act.isOut yield ag
    val leaders2: Set[Agent] =  for (ag,_,nxt)<-nexts2; act<-nxt if act.isOut yield ag
    // if there is more than a leading agent, then throw error.
    if (leaders1 ++ leaders2).size > 1
      then return Left(s"Multiple leaders found: ${(leaders1++leaders2).mkString(",")}")
    // if there are no leaders, all is good
    if leaders1.isEmpty && leaders2.isEmpty then return Right(())

    val leader = (leaders1++leaders2).head

    //// Check 2: send actions have matching receive actions
    // collects all single ab!x in c1 with a matching ab!y in c2
    def matchSent(a1:Action,a2:Action): Option[(Agent,Agent,Msg,Msg)] = (a1,a2) match
      case (Out(a1,b1,m1),Out(a2,b2,m2)) if a1==a2 && b1==b2 && m1!=m2 => Some((a1,b1,m1,m2))
      case _ => None
    // set of ab!x and ab!y matching actions from 2 options, returning (a,b,x,y) ---> should return (a,b,X,Y}
    def findMatch(a:Action, as: Set[Action]): List[(Agent,Agent,Msg,Msg)] =
      println(s"finding matches for $a in $as.")
      val res = as.toList.map(a2 => matchSent(a,a2).toList).flatten
      println(s"found $res")
      res
    // returns all matches after traversing nexts of C1
    println(s"nexts1: $nexts1")
    println(s"nexts2: $nexts1")
    val matches: Set[List[(Agent,Agent,Msg,Msg)]] =
      for (_,_,nxt1)<-nexts1 if nxt1.size == 1 && nxt1.head.isOut yield
        findMatch(nxt1.head,nexts2.flatMap(_._3))
    val matchesf = matches.flatten
    // checks if there is only 1 such a->b:x --> only a->b:x1,x2,...
    if matchesf.isEmpty then return Left(s"No pair of leading sends found. [${
      nexts1.flatMap(_._3).mkString(",")}] + [${nexts2.flatMap(_._3).mkString(",")}]")

    println(s"--- matchesf: $matchesf")
    println(s"--- nxt1: $nexts1") // 10b: c->b:y + c->b:y is discarded because y is the same...
    println(s"--- nxt2: $nexts2")
    if matchesf.size != 1 then return Left(s"Found multiple (or none) leading messages: [${
      matchesf.map(x=>s"${x._1}->${x._2}:${x._3}").mkString(",")}]")

    //// Check 3:
    // checks if all c1 and c2 projections are either for a->b:x or the same as a neighbour
    val (a,b,m1,m2):(Agent,Agent,Msg,Msg) = matchesf.head
    println(s"matchesf: $matchesf")
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
      then return Left(s"Agent $ag can do ${nxt.mkString(",")} in ${c2} and is not matched in $c1")
    // No error found. All is good.
    Right(())


  /**
   * New version of Well-Branchness.
   * For all minimums of C1 and C2
   *  - they are all sending actions from the same agent "a" (leader)
   *  - C1 can send to a set of "b!x", C2 can send to a set of "c!y"
   *    * b's and c's must be the same
   *    * x's and y's of matching agents must be different
   * @param c1 Choice 1
   * @param c2 Choice 2
   * @param sos how to evolve choreographies
   * @return a Result that is Right() or Left(reason)
   */
  def wellBranched(c1: Choreo, c2: Choreo)(using sos:SOS[Action,Choreo]): Result =
    val nxt1 = sos.next(c1).map(_._1) // set of global next actions
    val nxt2 = sos.next(c2).map(_._1)
    val leaders = for Out(a,_,_) <- (nxt1++nxt2) yield a
    if leaders.isEmpty then return Left(s"No sending actions ready in [$c1] nor [$c2].")
    if leaders.size > 1 then return Left(s"Multiple leaders [${leaders.mkString(",")}] found in [$c1] and [$c2].")
    val leader = leaders.head

    // collect actions from projections (agent, choreo, actions) restricted to sending leads
    def getNextRcv(c:Choreo, snd:Set[(Agent,Msg)]): Set[(Agent,Msg)] =
      for
        (a,ch) <- ChorNoTauProj.allAProj(c)
        (In(_,`leader`,m),_) <- sos.next(ch) if snd contains (a,m)
      yield
        a->m
    val snd1 = for Out(_,b,m) <- nxt1 yield b->m
    val snd2 = for Out(_,b,m) <- nxt2 yield b->m
    val rec1 = getNextRcv(c1,snd1)
    val rec2 = getNextRcv(c2,snd2)

    //all global sends have a local leading receive
    for s <- snd1 if !rec1.contains(s) do return Left(s"Message $leader->${s._1}${s._2.pp} in [$c1] cannot be readily received.")
    for s <- snd2 if !rec2.contains(s) do return Left(s"Message $leader->${s._1}${s._2.pp} in [$c2] cannot be readily received.")

    //b's and c's must be the same, but with different messages
    // i.e., each receiving "b:x" in c1 must have a matching "b:y" with a different "y" (and vice versa)
    for (b,x) <- rec1 do
      if rec2 contains (b,x) then return Left(s"Message $leader->$b${x.pp} can be taken in either options [$c1] and [$c2].")
      if !rec2.map(_._1).contains(b) then return Left(s"Message $leader->$b${x.pp} from [$c1] cannot be matched in [$c2].")
    // now vice-versa
    for (b,x) <- rec2 do
      if !rec1.map(_._1).contains(b) then return Left(s"Message $leader->$b${x.pp} from [$c2] cannot be matched in [$c1].")

    Right(())



