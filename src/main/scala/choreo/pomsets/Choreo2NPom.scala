package choreo.pomsets

import choreo.pomsets.NPomset.{Nesting, Order, empty}
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.syntax.Choreo._

object Choreo2NPom:

  private var seed:Int = 0
  private def next():Int = {seed+=1; seed-1}

  def apply(c:Choreo):NPomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p:NPomset = ps.foldRight(empty)(_>>_)
      updSeed(p)
    case Seq(c1, c2) => updSeed(apply(c1) >> apply(c2))
    case Par(c1, c2) => updSeed(apply(c1) ++ apply(c2))
    case Choice(c1, c2) => updSeed(apply(c1) or apply(c2))
    case End => empty
    //case Tau => empty //todo: check
    case act: Action =>
      val e = next()
      NPomset(Nesting(Set(e),Set()),Map(e->act),Set())
    case _ => sys.error("case not covered: $c")

  private def send(from:Agent, to:Agent, m:Msg):NPomset =
    val e1 = next()
    val e2 = next()
    NPomset(
      Nesting(Set(e1,e2),Set()),
      Map(e1->Out(from,to,m),e2->In(to,from,m)),
      Set(Order(e1,e2))
    )

  private def updSeed(p:NPomset): NPomset =
    seed = (p.events.toSet+(seed-1)).max+1
    p



