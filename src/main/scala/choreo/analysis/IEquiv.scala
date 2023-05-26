package choreo.analysis

import caos.sos.SOS
import choreo.syntax.{Agent, Choreo}
import Choreo.agents
import choreo.sos.ChorSyncSOS.Interact

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
  /** Gets the equivalence relation known by index `i`. */
  def getI[A,I](i:I)(using e:Equivs[A,I]): Equiv[A] = e.getOrElse(i,Map())
  /** Adds a pair to the equivalence (joining equiv. classes when needed) */
  def add[A](a:A,b:A)(using e:Equiv[A]): Equiv[A] =
    val all = get(a)++get(b)
    e ++ (for x<-all yield x->all)
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

  def showEq[A](eq: Equiv[A]): String =
    eq.values.toSet.map(cl => s"[${cl.mkString(" | ")}]").mkString(", ")

  def show[A,I](eqs:Equivs[A,I]): String =
    eqs.map((i,eq) => s"  '$i' -> ${showEq(eq)}").mkString("\n")

  def show[A,I](res:Result[Equivs[A,I]]): String = res match
    case Left(e) => s"Failed: $e"
    case Right(res) => show(res)


  ///////////////////////////
  // Building equivalences //
  ///////////////////////////

  /*% Algorithm
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
  /** Builds an equivalence relation that obeys the realisability  */
  def buildEquiv(qs:Set[St], sos:SOS[Lbl,St],all:Set[I],done:Set[St]): Result[Equivs[St,I]] =
//    println(s"next: ${qs.headOption} - Done: ${done.mkString(",")}")
    var equivs:Equivs[St,I] = Map()
    if qs.isEmpty then
//      println(s"no more states - stopping")
      return Right(equivs)
    val q = qs.head
    var more = qs-q
    if done(q) then
//      println("done - skipping")
      return buildEquiv(more,sos,all,done)

//    println(s"-- checking otions: ${sos.next(q).mkString(",")} (all: ${all.mkString(",")})")
    for (t,s) <- sos.next(q) do
//      println(s"---- maybe: by '$t' to '$s' (from:${t.from.mkString(",")}, to:${t.to.mkString(",")}")
      if !done(s) then more += s
    for (t,s) <- sos.next(q); i <- all--(t.to++t.from) do
//      println(s"---- found from '$i' by '$t' to '$s''")
      extend(q,s,i,sos)(using equivs) match
        case Right(upd) => equivs = upd
        case Left(err) => return Left(err)
      // add "s" to states to traverse

    // next state
    buildEquiv(more,sos,all,done+q) match
      case Right(rest) => Right(join(rest,equivs))
      case x => x


  def extendEq(q: St, s: St, i:I, sos: SOS[Lbl, St])(using eq: Equiv[St]): Result[Equiv[St]] =
    println(s"Expanding $s/$q")
    val q2s = sos.next(q).filter((t, _) => t.from(i) || t.to(i))
    val s2s = sos.next(s).filter((t, _) => t.from(i) || t.to(i))
    if q2s.map(_._1) != s2s.map(_._1) then
      return Left(s"Agent '$i' should not distinguish states:\n - q = $q\n - s = $s\nHowever:\n" +
        s" - q can do '${q2s.map(_._1).mkString(" / ")}'\n - s can do '${s2s.map(_._1).mkString(" / ")}'")

    var newEq = add(q, s)(using eq)
    for (tq, q2) <- q2s; (ts, s2) <- s2s if tq == ts do
      println(s" - both $q and $s can do $tq - adding $q2/$s2")
      extendEq(q2, s2, i, sos)(using newEq) match
        case Right(e) => newEq = e
        case err => return err

    Right(newEq)


  /** Given a pair of states that should be equivalent,  */
  def extend(q:St,s:St,i:Agent,sos:SOS[Lbl,St])(using eqs:Equivs[St,Agent]): Result[Equivs[St,Agent]] =
    extendEq(q,s,i,sos)(using getI(i)) match
      case Right(eq) => Right(eqs + (i->eq))
      case Left(err) => Left(err)

//    println("Expanding $s,q")
//    val q2s = sos.next(q).filter((t,_) => t.from(i) || t.to(i))
//    val s2s = sos.next(s).filter((t,_) => t.from(i) || t.to(i))
//    if q2s.map(_._1) != s2s.map(_._1) then
//      return Left(s"Agent '$i' should not distinguish states:\n - q = $q\n - s = $s\nHowever:\n"+
//        s" - q can do '${q2s.map(_._1).mkString(" / ")}'\n - s can do '${s2s.map(_._1).mkString(" / ")}'")
//
//    var iEquiv = add(q,s)(using getI(i))
//    for (tq,q2) <- q2s; (ts,s2) <- s2s if tq==ts do
//      println(s" - both $q and $s can do $tq - adding $q2/$s2")
//      iEquiv = add(q2,s2)(using iEquiv)
//    Right(eqs + (i->iEquiv))






