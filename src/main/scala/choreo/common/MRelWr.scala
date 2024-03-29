package choreo.common

/** MRel[A,B] is a relation implemented as a map to sets,
 * i.e., as Map[A,Set[B]], which is isomorphic to Set[(A,B)], indexed on A.
 */
class MRelWr[A,B]:
  /** Content: relation as a map from A to a set of B */
  val rel: Map[A,Set[B]] = Map()

  /** Add a new pair to the relation */
  def +(ab:(A,B)): MRelWr[A,B] =
    val (a,b) = ab
    if rel contains a
    then MRelWr(rel+(a->(rel(a)+b)))
    else MRelWr(rel+(a->Set(b)))

  /** Add a collection of pairs to the relation */
  def ++(abs:Iterable[(A,B)]): MRelWr[A,B] =
    abs.foldRight(this)((ab,prev)=> prev+ab)
  /** Add another MRel to the relation */
  def ++(abs:MRelWr[A,B]): MRelWr[A,B] =
    val rel2 = for (a,bs) <- abs.rel yield
      if rel contains a then a->(rel(a)++bs) else a->bs
    MRelWr(rel ++ rel2)

  /** Swap elements of the relation */
  def inverted:MRelWr[B,A] =
    var in = MRelWr[B,A]()
    for ((a,bs) <- rel; b<-bs)
      in = in + (b,a)
    in

  def toSet:Set[(A,B)] =
    rel.map({case (k,vs)=> vs.map(v=>(k,v))}).flatten.toSet



object MRelWr:
  /** Constructor for an empty relation */
  def apply[A,B](): MRelWr[A,B] = new MRelWr[A,B]()
  /** Constructor for a relation built from a map from A to sets of B */
  def apply[A,B](abs:Map[A,Set[B]]): MRelWr[A,B] = new MRelWr[A,B]():
    override val rel = abs
  /** Constructor of a singleton relation */
  def apply[A,B](ab:(A,B)): MRelWr[A,B] =
    apply(Map(ab._1->Set(ab._2)))
  /** Constructor for a collection of pairs */
  def apply[A,B](ab:Iterable[(A,B)]): MRelWr[A,B] =
    apply() ++ ab

  /**
   * Transitive closure of a relation
   * @param r the relation to be added the transitive closure
   * @return New relation extended with its transitive closure
   */
  def closure[A](r:MRelWr[A,A]): MRelWr[A,A] =
    val (a1,a2) = r.toSet.unzip
    closure(r, a1++a2)
  /**
   * Transitive closure of a relation
   * @param r the relation to be added the transitive closure
   * @param elems the set of elements that must have a transitive closure
   * @return New relation extended with its transitive closure
   */
  def closure[A](o:MRelWr[A,A], elems:Iterable[A]):MRelWr[A,A] =
    var tc = MRelWr[A,A]()
    def visit[A](from:A, to:A, cl:MRelWr[A,A], pred:MRelWr[A,A]):MRelWr[A,A] =
      var tc = cl + (from->to)
      for predec <- pred.rel.get(to) ; e<-predec ; if !tc.rel(from).contains(e) do
        tc = visit(from,e,tc,pred)
      tc

    for e<-elems do
      tc = visit(e,e,tc,o)
    //println(s"[closure] - of $o is:\n$tc")
    tc

  /**
   * Minimizes a relation by dropping pairs that can be inferred from a transitive closure
   * @param elems Elements to be traversed and possibly removed
   * @param ms Relation to be minimized
   * @tparam A type of the elements of the relation
   * @return Reduced relation without elements that can be inferred from a transitive closure
   */
  def reduction[A](elems:Set[A],ms:MRelWr[A,A]):MRelWr[A,A] =
    var reduced = ms.rel.map({case (k,v)=> (k,v-k)}) // remove reflexive
    def reachable(e:A):Set[A] =
      if !reduced.isDefinedAt(e) then Set()
      else reduced(e)++reduced(e).flatMap(e1=>reachable(e1))

    for (e1<-elems;e2<-elems; if reduced.contains(e1) && reduced(e1).contains(e2))  // && e1!=e2)
      reduced = reduced.updated(e1,reduced(e1)--reachable(e2))
    MRelWr(reduced)

  // todo: @Guille: maybe move these subTrees to DAG?
  def subTree[A](e:A, o:MRelWr[A,A]):MRelWr[A,A] =
    subTree(Set(e),o)
  //    var toVisit = o.getOrElse(e,Set())
  //    var ch:MS[A,A] = Map(e->toVisit)
  //    var visited = Set(e)
  //    ...

  def subTree[A](es:Set[A], o:MRelWr[A,A]):MRelWr[A,A] =
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
    MRelWr(sub)

