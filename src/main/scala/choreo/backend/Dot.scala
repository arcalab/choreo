package choreo.backend

/**
 * Created by guillecledou on 31/10/2020
 */

trait Dot[-A]:
  def toDot(elem:A):String

object Dot {
  implicit class DotOps[A](elem: A):
    def toDot(implicit dot: Dot[A]):String =
      dot.toDot(elem)
}