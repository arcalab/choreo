package choreo.realisability

import choreo.common.MRel
import choreo.realisability.Topology.Level
import choreo.npomsets.NPomset

/**
 * Created by guillecledou on 01/07/2021
 */

/** Top-down hierarchy between elements */
case class Topology[A](pred:MRel[A,A], succ:MRel[A,A], levels:Level[A]):
  lazy val init = pred.rel.collect({case (k,v) if v.isEmpty => k }).toSet
  lazy val predClosure = MRel.closure(pred,pred.rel.keySet) // todo: @Guille: can we drop the keySet (it will also include values)?
  lazy val succClosure = MRel.closure(succ,succ.rel.keySet)

  override def toString: String =
    levels.toString

  //def subtopology(e:A):Option[Topology[A]] =
  //  sub(e,Some(levels))
  //
  //protected def sub(e:A,levels:Option[Level[A]]):Option[Topology[A]] = levels match
  //  case Some(l) =>
  //    if l.elems.contains(e) then
  //      Some(Topology(pred,succ, Level(Set(e), children(e,l.next))))
  //    else sub(e,l.next)
  //  case _ => None
  //
  //protected def children(e:A,level: Option[Level[A]]):Option[Level[A]] =
  //  for l <- level yield
  //    Level(l.elems.filter(e1=>succClosure(e)),children(e,l.next))

object Topology:

  def apply[A]():Topology[A] = Topology[A](MRel(),MRel(),Level[A]())

  /**
   * Create a topology from an order between elements
   * @param pred predecesors of an element
   * @param succ succesors of an element
   * @tparam A type of the elements
   * @return a topology for the elements
   *
   * todo properly handle if a topology cannot be created
   */
  def apply[A](pred:MRel[A,A], succ:MRel[A,A]):Topology[A] =
    val inits: Set[A] = pred.rel.collect({case (k,v) if v.isEmpty => k }).toSet
    Topology(pred,succ,mkLevel(pred.rel,succ.rel,Level[A]().add(inits)))

  protected def mkLevel[A](pred:Map[A,Set[A]], succ:Map[A,Set[A]],top:Level[A]):Level[A] =
    var nt = Level[A]()
    var np = pred
    var ns = succ
    for e <- top.elems do
      val succe = ns.getOrElse(e,Set())
      ns += e->Set()
      np = np.map(n=> n._1 -> (n._2 - e))
      nt = nt.add(succe.filter(n=>np.getOrElse(n,Set()).isEmpty))
    if nt.elems.isEmpty then top
    else top.add(mkLevel(np,ns,nt))

  /**
   * Level inside a topology
   * @param elems elements in this level
   * @param next level below this level
   * @tparam A type of the elements
   */
  case class Level[A](elems:Set[A], next:Option[Level[A]]):
    def add(e:A):Level[A] = Level(elems+e,next)
    def add(es:Set[A]):Level[A] = Level(elems++es,next)
    def add(l:Level[A]):Level[A] = add(Some(l))
    def add(l:Option[Level[A]]):Level[A] = l match {
      case None => this
      case _ => next match
        case Some(l1) => l1.add(l)
        case None => Level(elems, l)
    }

    override def toString: String =
      elems.mkString(",") ++ "\n" ++
        (if next.isDefined then
          next.get.toString
        else "")

  object Level:
    def apply[A]():Level[A] = Level[A](Set[A](),None)