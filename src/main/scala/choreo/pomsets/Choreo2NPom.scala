package choreo.pomsets

import choreo.pomsets.NPomset.{Nesting, Order, empty}
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.syntax.Choreo._

object Choreo2NPom:

  private var seed:Int = 0
  private def next():Int = {seed+=1; seed-1}

  /** Converts a choreo expression into a Nested Pomset (with nested sets) */
  def apply(c:Choreo) =
    seed = 0
    choreo2npom(c)

  private def choreo2npom(c:Choreo):NPomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p:NPomset = ps.foldRight(empty)(_>>_)
      updSeed(p)
    case Seq(c1, c2)    => updSeed(choreo2npom(c1) >> choreo2npom(c2))
    case Par(c1, c2)    => updSeed(choreo2npom(c1) ++ choreo2npom(c2))
    case Choice(c1, c2) => updSeed(choreo2npom(c1) or choreo2npom(c2))
    case End => empty
    //case Tau => empty //todo: check
    case act: Action =>
      val e = next()
      NPomset(Nesting(Set(e),Set(),Set()),Map(e->act),Set())
    case Loop(c1) =>
      val p1 = choreo2npom(c1)
      //val p2 = choreo2npom(c1) // for now until having a renaming operation
      //NPomset.empty or (p1 >> NPomset(Nesting(Set(),Set(),Set(p2.events)),p2.actions,p2.order))
      NPomset(Nesting(Set(),Set(),Set(p1.events)),p1.actions,p1.order)
    case _ => sys.error(s"case not covered: $c")

  private def send(from:Agent, to:Agent, m:Msg):NPomset =
    val e1 = next()
    val e2 = next()
    NPomset(
      Nesting(Set(e1,e2),Set(),Set()),
      Map(e1->Out(from,to,m),e2->In(to,from,m)),
      Set(Order(e1,e2))
    )

  private def updSeed(p:NPomset): NPomset =
    seed = (p.events.toSet+(seed-1)).max+1
    p

