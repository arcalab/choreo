package choreo.realisability

import choreo.realisability.Topology

/**
 * Created by guillecledou on 01/07/2021
 */

/** Top-down hierarchy between elements */
case class Topology[A](elems:Set[A], next:Option[Topology[A]]):
  def add(e:A):Topology[A] = Topology(elems+e,next)
  def add(es:Set[A]):Topology[A] = Topology(elems++es,next)
  def add(l:Topology[A]):Topology[A] = add(Some(l))
  def add(l:Option[Topology[A]]):Topology[A] = l match {
    case None => this
    case _ => next match
      case Some(l1) => l1.add(l)
      case None => Topology(elems, l)
  }

object Topology:
  def apply[A]():Topology[A] = Topology[A](Set[A](),None)

  // todo properly handle if a topology cannot be created
  def apply[A](pred:Map[A,Set[A]], succ:Map[A,Set[A]]):Topology[A] =
    val inits: Set[A] = pred.collect({case (k,v) if v.isEmpty => k }).toSet
    apply(pred,succ,Topology[A]().add(inits))

  protected def apply[A](pred:Map[A,Set[A]], succ:Map[A,Set[A]],top:Topology[A]):Topology[A] =
    var nt = Topology[A]()
    var np = pred
    var ns = succ

    for e <- top.elems do
      val succe = ns.getOrElse(e,Set())
      ns += e->Set()
      np = np.map(n=> n._1 -> (n._2 - e))
      nt = nt.add(succe.filter(n=>np.getOrElse(n,Set()).isEmpty))

    if nt.elems.isEmpty then top
    else top.add(apply(np,ns,nt))