package choreo.backend


/**
 * Created by guillecledou on 01/11/2020
 */

trait Mermaid[A]:
  def toMermaid(elem:A):String

object Mermaid:
  implicit class MermaidOps[A](elem: A):
    def toMermaid(implicit m: Mermaid[A]):String =
      m.toMermaid(elem)
