package choreo.choreo2.view

/**
 * Created by guillecledou on 31/10/2020
 */

trait Dot[-A]:
  extension(e:A)
    def toDot:String

//object Dot {
//  implicit class DotOps[A](elem: A):
//    def toDot(implicit dot: Dot[A]):String =
//      dot.toDot(elem)