package choreo

/**
 * Created by guillecledou on 09/07/2021
 */

object Utils:

  def crossProduct[A](set:List[List[A]]):List[List[A]] = set match
    case Nil => List()
    case l::Nil => l.map(List(_))
    case l::ls => for e <- l ; cp <- crossProduct(ls) yield List(e) ++ cp

  def time[B](block: => B, msg: String): B =
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    println(s"[${msg}] - Elapsed time: " + (end - start) + "ns")
    result
