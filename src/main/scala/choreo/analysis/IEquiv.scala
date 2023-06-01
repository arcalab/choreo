package choreo.analysis

import caos.sos.SOS
import choreo.syntax.{Agent, Choreo, Msg}
import Choreo.agents
import choreo.sos.ChorSyncSOS.Interact

import scala.annotation.tailrec

object IEquiv:

  ///////////////////////////////////////////
  // Operations over equivalence relations //
  ///////////////////////////////////////////

  /** An equivalence is a relation AxA, modelled as a map A->2^A. */
  type Equiv[A] = Map[A,Set[A]]
  /** A family of equivalenves is an equivalence relation indexed by some A. */
  type Equivs[A,I] = Map[I,Equiv[A]]

  /** Gets the set of equivalent elements. */
  def get[A](a:A)(using e:Equiv[A]): Set[A] = e.getOrElse(a,Set(a))
  /** Gets the set of equivalent elements to `a` known by a given index `i`. */
  def get[A,I](a:A,i:I)(using e:Equivs[A,I]): Set[A] = get(a)(using e.getOrElse(i,Map()))
  /** Gets the equivalence relation known by index `i`. */
  def getI[A,I](i:I)(using e:Equivs[A,I]): Equiv[A] = e.getOrElse(i,Map())
  /** Adds a pair to the equivalence (joining equiv. classes when needed) */
  def add[A](a:A,b:A)(using e:Equiv[A]): Equiv[A] =
    if a==b then e else
      val all = get(a)++get(b)
      e ++ (for x<-all yield x->all)
  def add[A,I](a:A,b:A,i:I)(using e:Equivs[A,I]): Equivs[A,I] =
    e + (i -> add(a,b)(using getI(i)))
  /** Adds a group of elements to the equivalence. */
  def addClass[A](qs: Set[A])(using e: Equiv[A]): Equiv[A] =
    val all:Set[A] = qs.flatMap(get(_))
    e ++ (for x<-all yield x->all)
//    var res = eq
//    for q <- qs do
//      res += q -> (qs ++ res.getOrElse(q, Set()))
//    res

  /** Combines two equivalence relations, merging equivalence classes. */
  def addAll[A](e1:Equiv[A])(using e2:Equiv[A]): Equiv[A] =
    var res = e2
    for eqs <- e1.values do
      res = addClass(eqs)(using res)
    res
  /** Combines two families of equivalence classes, combining equivalences with the same index. */
  def join[A,I](e1:Equivs[A,I],e2:Equivs[A,I]): Equivs[A,I] =
    e1 ++ (for (i2,eq2) <- e2 yield
      if !e1.contains(i2) then i2->eq2
      else i2 -> addAll(e1(i2))(using eq2)
    )

  def showEq[A](eq: Equiv[A])(using pp:A=>String): String =
    eq.values.toSet.map(cl => s"[${cl.map(pp).mkString(",")}]").mkString(", ")

  def show[A,I](eqs:Equivs[A,I])(using pp:A=>String): String =
    eqs.map((i,eq) => s"  '$i' -> ${showEq(eq)}").mkString("\n")

  def show[A,I](res:Result[Equivs[A,I]])(using pp:A=>String): String = res match
    case Left(e) => s"Failed: $e"
    case Right(res) => show(res)

  def show[I](res: (Equivs[St, I],Trans))(using pp: St => String): String =
    s"Current equivalences:\n${show(res._1)}\nTransitions:\n${showT(res._2)}"

  def showT(t: Trans)(using pp: St => String): String =
    t.map((lb,sts) => s"  '$lb': ${sts.map((f,t)=>s"${pp(f)}>${pp(t)}").mkString(", ")}").mkString("\n")

//    res match
//    case Left(e) => s"Failed: $e"
//    case Right(res) => show(res)


  ///////////////////////////
  // Building equivalences //
  ///////////////////////////

  /*% Algorithm (OLD)
  % - collect I
  % - keep traversing from initial state until all states
  % - when q -t-> s
  %    - for i in I\t.I
  %      - add q =_i s
  %.       - for every q-t2->q2: s-t2->s2 must exist
  %.       - for every q-t2->q2 & s-t2->s2: add q2 =_i s2
  */

  // We restrict the search for an equivalence to LTS over Choreo (states) with Choreo labels (interactions),
  // and indexed over agents
  type St = Choreo
  type Lbl = Interact
  type I = Agent
  type Result[A] = Either[String,A]

  type Trans = Map[Lbl,Set[(St,St)]]
  def joinT(t1: Trans, t2: Trans): Trans =
    t2 ++ (for (m, as) <- t1
    yield m -> (t2.getOrElse(m, Set()) ++ as)
  )
  def groupMsg(ls:Iterable[Lbl]): Map[Msg,Set[Lbl]] =
    var res = Map[Msg,Set[Lbl]]()
    for l<-ls do
      res += (l.m -> (res.getOrElse(l.m,Set[Lbl]())+l))
    res

  /** Builds an equivalence relation that obeys the realisability  */
  def buildEquiv(qs:Set[St], sos:SOS[Lbl,St],all:Set[I],done:Set[St]): (Equivs[St,I],Trans) =
        //Result[Equivs[St,I]] =
//    println(s"next: ${qs.headOption} - Done: ${done.mkString(",")}")
    var equivs:Equivs[St,I] = Map()
    var trans:Trans = Map()
    if qs.isEmpty then
//      println(s"no more states - stopping")
      return (equivs,trans)
    val q = qs.head
    var more = qs-q
    if done(q) then
//      println("done - skipping")
      return buildEquiv(more,sos,all,done)

//    println(s"-- checking otions: ${sos.next(q).mkString(",")} (all: ${all.mkString(",")})")
    for (t,s) <- sos.next(q) do
//      println(s"---- maybe: by '$t' to '$s' (from:${t.from.mkString(",")}, to:${t.to.mkString(",")}")
      trans += t -> (trans.getOrElse(t,Set())+(q->s))
      if !done(s) then more += s
    for (t,s) <- sos.next(q); i <- all--(t.to++t.from) do
//      println(s"---- found from '$i' by '$t' to '$s''")
      equivs = extend(q,s,i)(using equivs)

    // next state
    buildEquiv(more,sos,all,done+q) match
      case (rest,trans2) => (join(rest,equivs),joinT(trans,trans2)) //Right(join(rest,equivs))


  def extendEqSimple(q: St, s: St, i:I, sos: SOS[Lbl, St])(using eq: Equiv[St]): Equiv[St] =
    add(q, s)(using eq)

//  def extendEq(q: St, s: St, i:I, sos: SOS[Lbl, St])(using eq: Equiv[St]): Equiv[St] =
//    //println(s"Expanding $s/$q")
//    val q2s = sos.next(q).filter((t, _) => t.from(i) || t.to(i))
//    val s2s = sos.next(s).filter((t, _) => t.from(i) || t.to(i))
//    // Dropping BAD condition below.
////    if q2s.map(_._1) != s2s.map(_._1) then
////      return Left(s"Agent '$i' should not distinguish states:\n - q = $q\n - s = $s\nHowever:\n" +
////        s" - q can do '${q2s.map(_._1).mkString(" / ")}'\n - s can do '${s2s.map(_._1).mkString(" / ")}'")
//
//    var newEq = add(q, s)(using eq)
//    for (tq, q2) <- q2s; (ts, s2) <- s2s if tq == ts do
//      //println(s" - both $q and $s can do $tq - adding $q2/$s2")
//      newEq = extendEq(q2, s2, i, sos)(using newEq)
//
//    newEq


  /** Given a pair of states that should be equivalent,  */
  private def extend(q:St,s:St,i:Agent/*,sos:SOS[Lbl,St]*/)(using eqs:Equivs[St,I]): Equivs[St,I] =
    //eqs + (i -> extendEq(q, s, i, sos)(using getI(i)))
    eqs + (i -> add(q, s)(using getI(i)))

  /** Initial attempt to check the Realisability Condition for a given LTS q.
   *  1. builds a family of core equivalence relations
   *  2. checks if all `glue` states can mimic equivalent transitions
   *  3. checks if all target states after mimicking the glue are equivalent (without extending the equivalences).
   */
  def checkRC(q: St, sos: SOS[Lbl, St])(using pp: St => String): Result[(Equivs[St, I], Trans)] =
    val (eqs, tr) = IEquiv.buildEquiv(Set(q), sos, Choreo.agents(q), Set())
    var errs = List[String]()
    for t <- tr do
      IEquiv.checkRC(t)(using eqs) match
        case Left("") =>
        case Right(e) =>
        case Left(err) => errs ::= err
    if errs.nonEmpty
    then
      Left("Failed. " + errs.mkString("\n---\n"))
    else Right(eqs -> tr)

  def checkRC(tr:(Lbl,Set[(St,St)]))(using eqs:Equivs[St,I], pp:St=>String): Result[Equivs[St,I]] =
    val as = (tr._1.from ++ tr._1.to).toList // fixing the list of agents
    val n = as.size
    if n<1 then return Left(s"Not supported transitions with no participants: ${tr._1}")
    val cs = combinations(tr._2,n)
    println(s"[${tr._1}]: ${//Math.pow(tr._2.size,n).toInt.max(n)-n
      cs.size} combinations (${tr._2.size} occurences, $n participants)")
    var updEqs:Equivs[St,I] = eqs
    for qs <- cs do
      val combs = as.zip(qs)
      val classes:List[Set[St]] = combs.map((a,q) => get(q._1,a))
      val gs = classes.tail.fold(classes.head)(_ intersect _) // intersecting all elements (assuming non-empty)
      for g <- gs do
        val g2s = for q<-tr._2 if q._1==g yield q._2
        if g2s.isEmpty
          // real error: no transition found for a glue
          then return Left(s"Glue ${pp(g)} failed - combinations: \n${
            combs.map((a,q)=>s" - $a: ${pp(q._1)}--${tr._1.m.names}-->${pp(q._2)}").mkString(";\n")}\n"+
            s"Expected ${pp(g)} to allow ${tr._1} since:\n  - ${
              combs.map((a,q)=>s"${pp(g)}={$a}${pp(q._1)}").mkString(" & ")}\n"+
            s"Searched for ${pp(g)} in sources of ${tr._2.map((x,y)=>pp(x)+"->"+pp(y)).mkString(", ")}"
        )
        if !g2s.exists(g2 => combs.forall((a,q) => get(g2,a)(using updEqs).contains(q._2)))
          // temporary error: need to extend the list
//          then
//            // selecting the first
//            val g2 = g2s.head
//            println(s"-- Selecting ${pp(g2)} (leaving ${(g2s-g2s.head).map(pp).mkString(",")}) s.t. ${
//              combs.map((a,q)=>s"${pp(g2)}={$a}${pp(q._2)}").mkString(" & ")
//            }")
//            updEqs = join(findExtra(Set(g2s.head),combs)(using updEqs), updEqs)
          then return Left(
            s"Found transitions from ${pp(g)} by ${tr._1} to ${g2s.map(pp).mkString(",")}, but none works:\n" +
              g2s.map( g2 => " - "+
                combs.map((a,q)=>s"${pp(g2)}=/={$a}${pp(q._2)}").mkString(" \\/ ")).mkString("\n") +
              "\n(wip to extend the equiv.)\n" +
              s"Suggestions of extensions:\n${show(findExtra(g2s,combs))}"
          )
    Right(updEqs) // all good!


  /** Checks the Realisability Condition for a given LTS q.
   *  1. builds a family of core equivalence relations
   *  2. checks if all `glue` states can mimic equivalent transitions
   *  3. extends the equivalence relations when the glue can be mimicked but the result is not equivalent.*/
  def checkRCExt(q: St, sos: SOS[Lbl, St])(using pp: St => String): Result[(Equivs[St, I], Trans)] =
    val (eqs, tr) = IEquiv.buildEquiv(Set(q), sos, Choreo.agents(q), Set())
    var eqs2 = eqs
    var last = eqs2
    var continue = true
    var res: Result[(Equivs[St, I], Trans)] = Left("Nothing done yet")
    var errs = List[String]()
    while continue do
      last = eqs2
      for t <- tr do
        IEquiv.checkRCExt(t)(using eqs2) match
          case Left("") =>
          case Left(err) => errs ::= err
          case Right(e) => eqs2 = e
      if errs.nonEmpty
      then
        res = Left("Failed. " + errs.mkString("\n---\n"))
        continue = false
      else if eqs2 == last
      then
        res = Right(eqs2 -> tr)
        continue = false
    // otherwise repeat, since new glues can exist (that must be checked)
    res

  def checkRCExt(tr: (Lbl, Set[(St, St)]))(using eqs: Equivs[St, I], pp: St => String): Result[Equivs[St, I]] =
    val as = (tr._1.from ++ tr._1.to).toList // fixing the list of agents
    val n = as.size
    if n < 1 then return Left(s"Not supported transitions with no participants: ${tr._1}")
    val cs = combinations(tr._2, n)
    println(s"[${tr._1}]: ${ //Math.pow(tr._2.size,n).toInt.max(n)-n
      cs.size
    } combinations (${tr._2.size} occurences, $n participants)")
    var updEqs: Equivs[St, I] = eqs
    for qs <- cs do
      val combs = as.zip(qs)
      val classes: List[Set[St]] = combs.map((a, q) => get(q._1, a))
      val gs = classes.tail.fold(classes.head)(_ intersect _) // intersecting all elements (assuming non-empty)
      for g <- gs do
        val g2s = for q <- tr._2 if q._1 == g yield q._2
        if g2s.isEmpty
        // real error: no transition found for a glue
        then return Left(s"Glue ${pp(g)} failed - combinations: \n${
          combs.map((a, q) => s" - $a: ${pp(q._1)}--${tr._1.m.names}-->${pp(q._2)}").mkString(";\n")
        }\n" +
          s"Expected ${pp(g)} to allow ${tr._1} since:\n  - ${
            combs.map((a, q) => s"${pp(g)}={$a}${pp(q._1)}").mkString(" & ")
          }\n" +
          s"Searched for ${pp(g)} in sources of ${tr._2.map((x, y) => pp(x) + "->" + pp(y)).mkString(", ")}"
        )
        if !g2s.exists(g2 => combs.forall((a, q) => get(g2, a)(using updEqs).contains(q._2)))
        // temporary error: need to extend the list
        then
          // selecting the first
          val g2 = g2s.head
          println(s"-- Selecting ${pp(g2)} (leaving {${(g2s-g2s.head).map(pp).mkString(",")}}) s.t. ${
            combs.map((a,q)=>s"${pp(g2)}={$a}${pp(q._2)}").mkString(" & ")
          }")
          updEqs = join(findExtra(Set(g2s.head),combs)(using updEqs), updEqs)
//        then return Left(
//          s"Found transitions from ${pp(g)} by ${tr._1} to ${g2s.map(pp).mkString(",")}, but none works:\n" +
//            g2s.map(g2 => " - " +
//              combs.map((a, q) => s"${pp(g2)}=/={$a}${pp(q._2)}").mkString(" \\/ ")).mkString("\n") +
//            "\n(wip to extend the equiv.)\n" +
//            s"Suggestions of extensions:\n${show(findExtra(g2s, combs))}"
//        )
    Right(updEqs) // all good!


  def findExtra(g2s:Set[St],combs:Iterable[(I,(St,St))])(using eqs:Equivs[St,I]): Equivs[St,I] =
    var newEquivs:Equivs[St,I] = Map()
    for g2<-g2s; (a,(_,to)) <- combs do
      if !get(g2,a).contains(to)
      then newEquivs = join(newEquivs,add(g2,to,a)(using eqs))
    newEquivs

//  def findExtra(g2s:Set[St],combs:List[(I,(St,St))])(using eqs:Equivs[St,I]): Equivs[St,I] =
//    for g2<-g2s do findExtra(g2,combs)(using eqs)

  /** Produces all sequences of size `size` of combinations of elements in `els`.
   *  (excluding when all elements are equal --> not any more) */
  def combinations[A](els:Iterable[A],size:Int): Set[List[A]] =
    combinations(els,size,Set(List[A]())) // --
      // (for e<-els yield List.fill(size)(e))

  @tailrec
  private def combinations[A](els:Iterable[A],size:Int,acc:Set[List[A]]): Set[List[A]] =
    if size<=0 then acc
    else combinations(els,size-1,acc.flatMap(lst => els.map(e=>e::lst)))


  // ------------------------

  /** Checks the Realisability Condition for a given LTS q.
   *  1. builds a family of core equivalence relations
   *     2. checks if all `glue` states can mimic equivalent transitions
   *     3. extends the equivalence relations when the glue can be mimicked but the result is not equivalent. */
  def checkRCTeamExt(q: St, sos: SOS[Lbl, St])(using pp: St => String): Result[(Equivs[St, I], Trans)] =
    val (eqs, tr) = IEquiv.buildEquiv(Set(q), sos, Choreo.agents(q), Set())
    var eqs2 = eqs
    var last = eqs2
    var continue = true
    var res: Result[(Equivs[St, I], Trans)] = Left("Nothing done yet")
    var errs = List[String]()
    val msgs = groupMsg(tr.keys) // store which labels exist for each MSG
    while continue do
      last = eqs2
      for t <- tr do
        IEquiv.checkRCTeamExt(t)(using eqs2, msgs, tr) match // now including msgs and tr to expand the combinations
          case Left("") =>
          case Left(err) => errs ::= err
          case Right(e) => eqs2 = e
      if errs.nonEmpty
      then
        res = Left("Failed. " + errs.mkString("\n---\n"))
        continue = false
      else if eqs2 == last
      then
        res = Right(eqs2 -> tr)
        continue = false
    // otherwise repeat, since new glues can exist (that must be checked)
    res

  def checkRCTeamExt(tr: (Lbl, Set[(St, St)]))
                    (using eqs: Equivs[St, I], msgs: Map[Msg,Set[Lbl]], trs: Trans, pp: St => String): Result[Equivs[St, I]] =
    val as = (tr._1.from ++ tr._1.to)
    val n = as.size
    if n < 1 then return Left(s"Not supported transitions with no participants: ${tr._1}")
    val combs = combinationsExt(/*tr._2*/tr._1, as)
    println(s"[${tr._1}]: " + //${ //Math.pow(tr._2.size,n).toInt.max(n)-n
//      combs.size
//    } combinations (${tr._2.size} occurences, $n participants)\n/--\n"+
      s"${tr._2.size} combinations for agents {${as.mkString(",")}}\n - ${
      combs.map(_.map((x,yz)=>(x,pp(yz._1)->pp(yz._2)))).mkString("\n - ")}")
    var updEqs: Equivs[St, I] = eqs
    for comb <- combs do
//      val combs = as.zip(qs)
      val classes: List[Set[St]] = comb.toList.map((a, q) => get(q._1, a))
      val gs = classes.tail.fold(classes.head)(_ intersect _) // intersecting all elements (assuming non-empty)
      for g <- gs do
        val g2s = for q <- tr._2 if q._1 == g yield q._2
        if g2s.isEmpty
        // real error: no transition found for a glue
        then return Left(s"Glue ${pp(g)} failed - combinations: \n${
          comb.map((a, q) => s" - $a: ${pp(q._1)}--${tr._1.m.names}-->${pp(q._2)}").mkString(";\n")
        }\n" +
          s"Expected ${pp(g)} to allow ${tr._1} since:\n  - ${
            comb.map((a, q) => s"${pp(g)}={$a}${pp(q._1)}").mkString(" & ")
          }\n" +
          s"Searched for ${pp(g)} in sources of ${tr._2.map((x, y) => pp(x) + "->" + pp(y)).mkString(", ")}"
        )
        if !g2s.exists(g2 => comb.forall((a, q) => get(g2, a)(using updEqs).contains(q._2)))
        then
          // selecting the first
          val g2 = g2s.head
          println(s"-- Selecting ${pp(g2)} (leaving {${(g2s - g2s.head).map(pp).mkString(",")}}) s.t. ${
            comb.map((a, q) => s"${pp(g2)}={$a}${pp(q._2)}").mkString(" & ")
          }")
          updEqs = join(findExtra(Set(g2s.head), comb)(using updEqs), updEqs)
    Right(updEqs) // all good!


  /** Produces all sequences of size `size` of combinations of elements in `els`
   * (not excluding when all elements are equal) */
  @tailrec
  private def combinationsExt(lbl: Lbl, agents: Set[I], acc: Set[Map[I, (St, St)]] = Set(Map()))
                             (using msgs: Map[Msg, Set[Lbl]], trs: Trans, pp: St => String): Set[Map[I, (St, St)]] =
    agents.headOption match
      case None => acc
      case Some(ag) =>
        def ok(l: Lbl) = if lbl.from(ag) then l.from(ag)
        else if lbl.to(ag) then l.to(ag) else false

        val lbls = msgs.getOrElse(lbl.m, Set()).filter(ok) // labels with message m and ag as in/out
        val sts = lbls.flatMap(lbl2 => trs.getOrElse(lbl2, Set()))
        // println(s"### adding (from $lbl) --> ${sts.map(st=>s"$ag->${pp(st._1)}/${pp(st._2)}")}") // $sts (msgs: ${msgs}) (${msgs.getOrElse(lbl.m,Set())})")
        val newAcc = acc.flatMap(mp => sts.map(st => mp + (ag -> st)))
        //println(s"### new acc: $newAcc")
        combinationsExt(lbl, agents - ag, newAcc)



