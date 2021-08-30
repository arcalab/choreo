package choreo.common

import scala.language.implicitConversions

/**
 * MRel compiles a set of functions to reason over Map[A,Set[B]] interpreted as a relation of AxB.
 */
object MRel:
  /** A relation AxB seen as a map from A to Set[B] */
  type MR[A,B] = Map[A,Set[B]]

  case class WrapMRel[A,B](rel:MR[A,B]):
    /** Reference to `add(abs)(rel)` */
    def :++(abs:Iterable[(A,B)]): MR[A,B] = add(abs)(using rel)
    /** Reference to `add(abs)(rel)` */
    def :++(abs:MR[A,B]): MR[A,B] = add(abs)(using rel)
    /** Reference to `add(ab)(rel)` */
    def :+(ab:(A,B)): MR[A,B] = add(ab)(using rel)

  implicit def relToWrap[A,B](rel:MR[A,B]): WrapMRel[A,B] = WrapMRel(rel)

  def mkMR[A,B](rel:(A,B)*): MR[A,B] = add(rel)(using Map())
  def mkMR[A,B](rel:Iterable[(A,B)]): MR[A,B] = add(rel)(using Map())

  /** Add a set of pairs to a relation */
  def add[A,B](abs:Iterable[(A,B)])(using rel:MR[A,B]): MR[A,B] =
    abs.foldRight(rel)((ab,prev)=> add(ab)(using prev))

  /** Join 2 relations */
  def add[A,B](m2:MR[A,B])(using m1:MR[A,B]): MR[A,B] =
    val m2b = for (a,bs) <- m2 yield
      if m1 contains a then a->(m1(a)++bs) else a->bs
    m1 ++ m2b

  /** Add an element to a relation */
  def add[A,B](ab:(A,B))(using rel:MR[A,B]): MR[A,B] =
    val (a,b) = ab
    if rel contains a
    then rel+(a->(rel(a)+b))
    else rel+(a->Set(b))


  /** Swap elements of the relation */
  def invert[A,B](using rel:MR[A,B]): MR[B,A] =
    var res: MR[B,A] = Map()
    for ((a,bs) <- rel; b<-bs)
      res = res :+ (b,a)
    res

  /** Relation as a set of pairs */
  def asPairs[A,B](using rel:MR[A,B]):Set[(A,B)] =
    rel.map({case (k,vs)=> vs.map(v=>(k,v))}).flatten.toSet


  /**
   * Transitive closure of a relation
   * @param r the relation to be added the transitive closure
   * @return New relation extended with its transitive closure
   */
  def closure[A](using r:MR[A,A]): MR[A,A] =
    val (a1,a2) = asPairs.unzip
    closure(a1++a2)
  /**
   * Transitive closure of a relation
   * @param r the relation to be added the transitive closure
   * @param elems the set of elements that must have a transitive closure
   * @return New relation extended with its transitive closure
   */
  def closure[A](elems:Iterable[A])(using r:MR[A,A]): MR[A,A] =
    var tc: MR[A,A] = Map()
    def visit[A](from:A,to:A,cl:MR[A,A],pred:MR[A,A]): MR[A,A] =
      var tc = cl :+ (from->to)
      for predec <- pred.get(to) ; e<-predec ; if !tc(from).contains(e) do
        tc = visit(from,e,tc,pred)
      tc

    for e<-elems do
      tc = visit(e,e,tc,r)
    //println(s"[closure] - of $o is:\n$tc")
    tc



  /**
   * Minimizes a relation by dropping pairs that can be inferred from a transitive closure
   * @param elems Elements to be traversed and possibly removed
   * @param ms Relation to be minimized
   * @tparam A type of the elements of the relation
   * @return Reduced relation without elements that can be inferred from a transitive closure
   */
  def reduction[A](elems:Set[A])(using ms:MR[A,A]): MR[A,A] =
    var reduced = ms.map({case (k,v)=> (k,v-k)}) // remove reflexive
    def reachable(e:A):Set[A] =
      if !reduced.isDefinedAt(e) then Set()
      else reduced(e)++reduced(e).flatMap(e1=>reachable(e1))

    for (e1<-elems;e2<-elems; if reduced.contains(e1) && reduced(e1).contains(e2))  // && e1!=e2)
      reduced = reduced.updated(e1,reduced(e1)--reachable(e2))
    reduced


  // todo: @Guille: maybe move these subTrees to DAG?
  def subTree[A](e:A)(using o:MR[A,A]):MR[A,A] =
    subTree(Set(e))
  //    var toVisit = o.getOrElse(e,Set())
  //    var ch:MS[A,A] = Map(e->toVisit)
  //    var visited = Set(e)
  //    ...

  def subTree[A](es:Set[A])(using o:MR[A,A]):MR[A,A] =
    var sub = es.map(e=>e->o.rel.getOrElse(e,Set())).toMap
    var toVisit:Set[A] = sub.flatMap(_._2).toSet
    var visited = es
    while toVisit.nonEmpty do
      val n = toVisit.head
      if (!visited.contains(n)) then
        val cn = o.rel.getOrElse(n,Set())
        toVisit ++= cn
        visited+=n
        sub += n->cn
      toVisit  -=n
    sub
