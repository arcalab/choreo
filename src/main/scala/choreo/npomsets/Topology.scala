package choreo.npomsets

/**
 * Created by guillecledou on 01/07/2021
 */

/** Top-down hierarchy between elements */
case class Topology[A](elems:Set[A], next:Option[Topology[A]]):
  def add(e:A):Topology[A] = Topology(elems+e,next)
  def add(l:Topology[A]):Topology[A] = add(Some(l))
  def add(l:Option[Topology[A]]):Topology[A] = l match {
    case None => this
    case _ => next match
      case Some(l1) => l1.add(l)
      case None => Topology(elems, l)
  }

object Topology:
  def apply[A]():Topology[A] = Topology[A](Set[A](),None)
  
  