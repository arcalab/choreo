package choreo.realisability

import cats.data.StateT
import cats.implicits.*
import choreo.npomsets.NPomset
import choreo.npomsets.NPomset.{Actions, Event, NChoice, Nesting, Order}
import choreo.common.MRel
import choreo.common.MRel.*
import choreo.syntax.Choreo


/**
 * Created by guillecledou on 14/09/2021
 */

object Merge:

  case class MergeSt(ic:Order, order:Order
                     , actions:Actions,seed:Int):
    def updSeed(ns:Int)       = MergeSt(ic, order, actions, ns)
    def updOrd(o:Order)       = MergeSt(ic, o, actions, seed)
    def updIC(o:Order)        = MergeSt(o, order, actions, seed)
    def updActs(acts:Actions) = MergeSt(ic, order, acts, seed)

    lazy val pred = MRel.closure(using (order:++ic))

  case class CompRes(comp:Option[Choice],potential:Choices,merged:Choices)

  type NestOrChoice = NChoice[Event] | Nesting[Event]
  type ErrorOr[A]   = Either[String,A]
  type Res[A]       = StateT[ErrorOr,MergeSt,A]
  type Choices      = Set[NChoice[Event]]
  type Choice       = NChoice[Event]
  type Arrows       = Set[(Event,Event)]

  /**
   * Get the current state
   * @return
   */
  def get():Res[MergeSt] =
    for
      st <- StateT.get[ErrorOr,MergeSt]
    yield st

  /**
   * Modify the current state
   * @param f
   * @return
   */
  def modify(f:MergeSt=>MergeSt):Res[Unit] =
    for
      _ <- StateT.modify[ErrorOr,MergeSt](f)
    yield ()

  /**
   * Wrap an A result in Res monad.
   * @param r
   * @tparam A
   * @return
   */
  def pure[A](r:A):Res[A] = StateT.pure[ErrorOr,MergeSt,A](r)

  /**
   * Main compose function.
   * Tries to compose choices in an interclosure
   *
   * @param ic
   * @return todo: change NPomset res to ErrorOR[NPomset]
   */
  def compose(ic:Interclosure):NPomset =
    val npom = ic.getNPom
    val seed = npom.events.toSet.max + 1
    var initSt = MergeSt(ic.ic,ic.getNetNPom.pred,npom.actions,seed)

    val res:ErrorOr[(MergeSt,Choices)] = compose(npom.events.choices).run(initSt)

    res match
      case Left(err) => println(err); ic.getNPom
      case Right((st,comp)) =>
        NPomset(Nesting(npom.events.acts,comp,npom.events.loops)
          , st.actions
          , MRel.add(st.order)(using st.ic)
          , npom.loop)

  /**
   * Try to compose a set of choices, in every possible direction
   * @param choices
   * @return
   */
  protected def compose(choices:Choices):Res[Choices] =
    for
      res <- composeAll(choices,Set())
    yield res

  /**
   * Try to compose al choices in todo, except those that were already tried.
   * @param todo
   * @param tried
   * @return
   */
  protected def composeAll(todo:Choices,tried:Choices):Res[Choices] =
    if (todo--tried).size > 0 then
      val next = (todo--tried).head
      //println(s"[composeALL] - tried: $tried\n todo: $todo")
      for
        res <- compose(next,todo-next)
        //_ = println(s"[composeALL] - trying with : $next\n and: ${todo - next}")
        rest <- composeAll(res,tried+next)
      yield rest
    else
      pure[Choices](todo)

  /**
   * Try to compose each choice in from, with choices in to
   * @param cs1
   * @param cs2
   * @return
   */
  protected def compose(cs1:Choices,cs2:Choices):Res[Choices] =
    compose(cs1++cs2)

  /**
   * Try to compose c with choices in to
   * @param c
   * @param to
   * @return
   */
  protected def compose(c:Choice,to:Choices):Res[Choices] =
    for
      potential <- findPotential(c,to)
      compatible <- findMergeable(c,potential,to)
      //_ = println(s"[compose(c,cs)] - potential = $potential")
      //_ = println(s"[compose(c,cs)] - compatible = $compatible")
      compatibleChoices = compatible.map(_.to)
      keep = potential -- compatibleChoices
      //_ = println(s"[compose(c,cs)] - keep = $keep")
      comp <- if keep.nonEmpty then
        StateT.liftF[ErrorOr,MergeSt,Choices](Either.left(s"Cannot compose ${c} with ${keep.mkString("\n")}"))
      else if compatible.nonEmpty then
        composeMergeable(c,compatible)
      else pure(to+c)
      rest = to--compatibleChoices
    yield if rest.isEmpty then comp else comp ++ rest

  def findPotential(c:Choice,to:Choices):Res[Choices] =
    for
      st <- get()
      res = to.toList.filter(c1 => hasIC(c,c1,st.ic))
    yield res.toSet

  def hasIC(c1:Choice,c2:Choice,ic:Order):Boolean = 
    val (ev1,ev2) = (c1.toSet,c2.toSet)
    val res = ic.exists(kv => ev2.contains(kv._1) && ev1.intersect(kv._2).nonEmpty)
    //println(s"[hasIC] - c1:$c1")
    //println(s"[hasIC] - c2:$c2")
    //println(s"[hasIC] - ic:$ic")
    //println(s"[hasIC] - res:$c1")
    res

  def composeMergeable(c:Choice,mergeable:Set[Mergeable]):Res[Choices] =
    for
      res <- mergeable.toList.traverse(m=>composeMergeable(c,m))
    yield res.toSet

  def composeMergeable(c:Choice,m:Mergeable):Res[Choice] =
    for
      st <- get()
      comp <- composeChoice(c,m.to,m.by)
    yield comp

  def composeChoice(c1:Choice,c2:Choice,by:Rel):Res[Choice] = by match
    case BRel(true) =>
      for
        n1 <- composeNesting(c1.left,c2.right)
        n2 <- composeNesting(c1.right,c2.left)
      yield NChoice(n1,n2)
    case BRel(false) =>
      for
        n1 <- composeNesting(c1.left,c2.left)
        n2 <- composeNesting(c1.right,c2.right)
      yield NChoice(n1,n2)

  def composeNesting(n1:Nesting[Event],n2:Nesting[Event]):Res[Nesting[Event]] =
    for
      comp <- compose(n1.choices++n2.choices)
      st <- get()
      icPairs <- arrows(n1,n2)
      //_ = println(s"[composeNesting] - ic = ${st.ic}")
      //_ = println(s"[composeNesting] - order = ${st.order}")
      //_ = println(s"[composeNesting] - arrows between n1: $n1 \nand \nn2:$n2 \n= ${icPairs}")
      ic = MRel.mkMR(icPairs)
      //_ = println(s"[composeNesting] - toChange: ${ic}")
      //_ = println(s"[composeNesting] - new order: ${st.order:++ic}")
      //_ = println(s"[composeNesting] - new ic: ${MRel.rm(ic)(using st.ic)}")
      st <- get()
      // add ic to global order
      _ <- modify(_.updOrd(st.order:++ic))
      // remove ic from global ic
      _ <- modify(_.updIC(MRel.rm(ic)(using st.ic)))
    yield Nesting(n1.acts++n2.acts,comp,n1.loops++n2.loops)

  def arrows(n1:Nesting[Event],n2:Nesting[Event]):Res[Arrows] =
    for
      st <- get()
      pairs = MRel.asPairs(using st.ic)
      (ev1,ev2) = (n1.toSet,n2.toSet)
    yield pairs.collect({case (s,p) if ev1.contains(p) && ev2.contains(s) => (s,p)})

  /**
   * Find mergeable choices to merge with c
   * @param c
   * @param to
   * @return
   */
  def findMergeable(c:Choice, to:Choices, all:Choices):Res[Set[Mergeable]] =
    for
      res <- to.toList.traverseFilter(c1=>isMergeable(c,c1,to++all-c1))
    yield res.toSet

  /**
   * Checks if c1 and c2 are mergeable, in which case it returns
   * the relation by which they can merge.
   * To be meargable, in addition, c2 cannot be a potential merge with others
   * @param c1
   * @param c2
   * @param others
   * @return
   */
  def isMergeable(c1:Choice,c2:Choice,others:Choices):Res[Option[Mergeable]] =
    for
      ll <- isMergeable(c1.left,c2.left)
      lr <- isMergeable(c1.left,c2.right)
      rl <- isMergeable(c1.right,c2.left)
      rr <- isMergeable(c1.right,c2.right)
      conflictC2out <- hasReadyOutgoinOuts(c2,others+c1)
      conflictC2in <- hasReadyIncomingOuts(c2,others)
      conflictC1 <- hasReadyIncomingOuts(c1,others-c1-c2)
      conflict = conflictC2out || conflictC2in || conflictC1
      _ = println(s"[isMergeable] - c1: ${c1}\nc2: ${c2}")
      _ = println(s"[isMergeable] - ll: ${ll}\n lr: ${lr}\n rl: ${rl}\n rr: ${rr}")
      _ = println(s"[isMergeable] - conflictC2out: $conflictC2out")
      _ = println(s"[isMergeable] - conflictC2in: $conflictC2in")
      _ = println(s"[isMergeable] - conflictC1: $conflictC1")
    yield if ll && rr && !lr && !rl && !conflict then
      Some(Mergeable(c1,c2,BRel(false)))//,arrows(c1,c2)))
    else if lr && rl && !ll && !rr && !conflict then
      Some(Mergeable(c1,c2,BRel(true)))//,arrows(c1,c2)))
    else None

  def isMergeable(n1:Nesting[Event],n2:Nesting[Event]):Res[Boolean] =
    for
      st <- get()
      //emptyTop = n1.acts.isEmpty || n2.acts.isEmpty // some top level is has no actions
      //mergeable <- hasTopReadyIC(n1,n2).orElse(emptyTop && hasReadyIC(n1.choices,n2.choices))
      //icNested <- hasReadyIC(n1.choices,n2.choices) // some ic ready between choices of n1 and n2
      icTop <- hasReadyIC(n1,n2) // has some ic ready
    yield icTop || (n1.toSet.isEmpty && n2.toSet.isEmpty)



  //def isMergeable(c1:Choice,c2:Choice,others:Choices):Res[Option[Mergeable]] =
  //  for
  //    arr <- canMerge(c1,c2)
  //  yield if arr.nonEmpty && others.forall(c=>canMerge(c,c2).isEmpty && canMerge(c2,c).isEmpty) then
  //    findRel(c1,c2)
  //  else None

  /**
   * Checks if two nestings have an ic between them at the top level
   * @param n1
   * @param n2
   * @return
   */
  def hasTopReadyIC(n1:Nesting[Event], n2:Nesting[Event]):Res[Boolean] =
    for
      st <- get()
      outs <- nextOuts(n1)
      ins <- nextIns(n2)
      topOuts = outs.intersect(n1.acts)
      topIns = ins.intersect(n2.acts)
    yield topIns.exists(i=>st.ic.getOrElse(i,Set()).intersect(topOuts).nonEmpty)

  /**
   * Checks if two choices have some interclosure relation (at some level)
   * between them that can merge ready to be merged
   * @param c1
   * @param c2
   * @return
   */
  def hasReadyIC(c1:Choice,c2:Choice):Res[Boolean] =
    for
      st <- get()
      outs <- nextOuts(c1)
      ins <- nextIns(c2)
    yield ins.exists(i=>st.ic.getOrElse(i,Set()).intersect(outs).nonEmpty)

  def hasReadyIC(n1:Nesting[Event], n2:Nesting[Event]):Res[Boolean] =
    for
      st <- get()
      outs <- nextOuts(n1)
      ins <- nextIns(n2)
    yield ins.exists(i=>st.ic.getOrElse(i,Set()).intersect(outs).nonEmpty)

  def hasReadyIncomingOuts(c:Choice,from:Choices):Res[Boolean] =
    for
      st <- get()
      outs <- from.toList.traverse(c1=>nextOuts(c1))
      ins = c.toSet.filter(e=>chor2Act(st.actions(e)).isIn)
      flat = outs.flatten.toSet
    yield ins.exists(i => st.ic.getOrElse(i,Set()).intersect(flat).nonEmpty)

  def hasReadyOutgoinOuts(c:Choice,from:Choices):Res[Boolean] =
    for
      st <- get()
      ins = from.toList.view.map(c1=>c1.toSet).flatten
      outs <- nextOuts(c)
    yield ins.exists(i => st.ic.getOrElse(i,Set()).intersect(outs).nonEmpty)

  /**
   * Checks if two choices have some interclosure relation (at some level)
   * between them that can merge ready to be merged
   * @param c1
   * @param c2
   * @return
   */
  def hasReadyIC(cs1:Choices,cs2:Choices):Res[Boolean] =
    val l1 = cs1.toList
    val l2 = cs2.toList
    l1.existsM(c1 => l2.existsM(c2=>hasReadyIC(c1,c2)))

  /**
   * Set of events in a nesting that are ready to be merged
   * @return
   */
  def next(n:Nesting[Event]):Res[Set[Event]] =
    for
      st <- get()
      um <- unmerged()
      // unmerged events in n
      umN = um.intersect(n.toSet) // todo: check this or n.acts
      res <- umN.toList.traverseFilter(e=>filterUnmergedPred(e,n))
    yield res.toSet // unmerged event without unmerged predecesors

  /**
   * Reterns e if e has no unmerged predecesors in n
   * @param e
   * @param n
   * @return
   */
  def filterUnmergedPred(e:Event,n:Nesting[Event]):Res[Option[Event]] =
    for
      umPred <- unmergedPred(e,n)
    yield if (umPred-e).isEmpty then Some(e) else None

  /**
   * Set of events that are outputs in n.acts and are ready to be merged.
   * Basically, every output that appears in the interclosure, and
   * such that it has no unmerged predecesors.
   * @param n
   * @return
   */
  def nextOuts(n:Nesting[Event]):Res[Set[Event]] =
    for
      st <- get()
      next <- next(n)
    yield next.filter(e=>chor2Act(st.actions(e)).isOut)

  /**
   * Set of events that are inputs in n.acts and are ready to be merged.
   * Basically, every input that appears in the interclosure, and
   * such that it has no unmerged predecesors beyond its natural matching outputs.
   * @param n
   * @return
   */
  def nextIns(n:Nesting[Event]):Res[Set[Event]] =
    for
      st <- get()
      next <- next(n)
    yield next.filter(e=>chor2Act(st.actions(e)).isIn)

  /**
   * Set of events that are outputs in c and are ready to be merged.
   * @param c
   * @return
   */
  def nextOuts(c:Choice):Res[Set[Event]] =
    for
      ol <- nextOuts(c.left)
      or <- nextOuts(c.right)
    yield ol++or

  /**
   * Set of events that are inputs in c and are ready to be merged.
   * @param c
   * @return
   */
  def nextIns(c:Choice):Res[Set[Event]] =
    for
      il <- nextIns(c.left)
      ir <- nextIns(c.right)
    yield il++ir

  /**
   * Set of events that haven't been merged or tried to be merged yet
   * @return
   */
  def unmerged():Res[Set[Event]] =
    for
      st <- get()
    yield toSet(st.ic)

  /**
   * Checks if an event has unmerged predecesors
   * @param e event
   * @return
   */
  def unmergedPred(e:Event,n:Nesting[Event]):Res[Set[Event]] =
    for
      st <- get()
      um <- unmerged()
      pred = st.pred.getOrElse(e,Set()).intersect(n.toSet) //todo: check st.ic.getOrElse(e,Set())
      // if e is an input, remove its matching outputs
      pred_ = if chor2Act(st.actions(e)).isIn then
        pred.filterNot(e1=>chor2Act(st.actions(e1)).matchingOI(chor2Act(st.actions(e))))
      else pred
    yield pred_.intersect(um)

  /**
   * Relations between choices
   */
  trait Rel

  /**
   * a bijection relation
   */
  case class BRel(cross:Boolean) extends Rel

  ///**
  // * a sub bijection relation
  // */
  //case class SubBRel(cross:Boolean) extends Rel

  /**
   * An ambigous relation, from every a in A to every b in B
   */
  case object ARel extends Rel

  ///**
  // * A sub ambigous relation, from every a in A to every b in B
  // */
  //case object SubARel extends Rel

  /**
   * Two mergeable choices by a given relation
   */
  case class Mergeable(from:Choice,to:Choice,by:Rel)//,arrows:Arrows)

  def chor2Act(c: Choreo): Choreo.Action = c match
    case a: Choreo.Action => a
    case _ => sys.error(s"Unsupported analysis of complex actions ($c) in realisability.Merge")


