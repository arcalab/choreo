package choreo.npomsets

import NPomset._
import choreo.common.MRel._
import choreo.syntax.Choreo._
import choreo.syntax.{Agent, Choreo, Msg}

object Choreo2NPom:

  /** Converts a choreo expression into a Nested Pomset (with nested sets) */
  def apply(c: Choreo):NPomset =
    (new Choreo2NPom).choreo2npom(c)

class Choreo2NPom:

  private var seed:Int = 1
  private def next():Int = {seed+=1; seed-1}

  /** Converts a choreo expression into a Nested Pomset (with nested sets) */
//  def apply(c:Choreo) =
//    seed = 1 // fragile in concurrent code
//    choreo2npom(c)

  private def choreo2npom(c:Choreo):NPomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p:NPomset = ps.foldRight(empty)(_ ++ _)
      updSeed(p)
    case Internal(a, m) =>
      val p = internal(a, m)
      updSeed(p)
    case Seq(c1, c2)    => updSeed(choreo2npom(c1) >> choreo2npom(c2))
    case Par(c1, c2)    => updSeed(choreo2npom(c1) ++ choreo2npom(c2))
    case Choice(c1, c2) => updSeed(choreo2npom(c1) or choreo2npom(c2))
    case End => empty
    //case Tau => empty //todo: check
    case act: Action =>
      val e = next()
      NPomset(Nesting(Set(e),Set(),Set()),Map(e->act),Map(),(Map(),0))
    case Loop(c1) =>
      val p1 = choreo2npom(c1)
      val lOrder = for  a<-p1.events.toSet
                        b<-p1.events.toSet
                        ag<-agents(p1.actions(a))
                        bg<-agents(p1.actions(b))
                        if ag==bg && a!=b // force a!=b to drop reflexive part (inferable)
      yield
        (a,b)
      NPomset(Nesting(Set(),Set(),Set(p1.events)),p1.actions,p1.pred, (mkMR(lOrder),seed))
    case _ => sys.error(s"case not covered in chreo2npom: $c")

  private def send(from:Agent, to:Agent, m:Msg):NPomset =
    val e1 = next()
    val e2 = next()
    NPomset(
      Nesting(Set(e1,e2),Set(),Set()),
      Map(e1->Out(from,to,m),e2->In(to,from,m)),
      mkMR(e2->e1),
      (Map(),seed)
    )

  private def internal(ag:Agent, m:Msg):NPomset =
    val e1 = next()
    NPomset(
      Nesting(Set(e1),Set(),Set()),
      Map(e1->Internal(ag,m)),
      Map(),
      (Map(),seed)
    )

  private def updSeed(p:NPomset): NPomset =
    seed = (p.events.toSet+(seed-1)).max+1
    p


//  private def choreo2syncnpom(c: Choreo): NPomset = c match
//    case Send(as, bs, m) =>
//      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
//      val p: NPomset = ps.foldRight(empty)(_ ++ _)
//      updSeed(p)
//    case Internal(a, m) =>
//      val p = internal(a, m)
//      updSeed(p)
//    case Seq(c1, c2) => updSeed(choreo2syncnpom(c1) >> choreo2syncnpom(c2))
//    case Par(c1, c2) => updSeed(choreo2syncnpom(c1) ++ choreo2syncnpom(c2))
//    case Choice(c1, c2) => updSeed(choreo2syncnpom(c1) or choreo2syncnpom(c2))
//    case End => empty
//    //case Tau => empty //todo: check
//    case act: Action =>
//      val e = next()
//      NPomset(Nesting(Set(e), Set(), Set()), Map(e -> act), Map(), (Map(), 0))
//    case Loop(c1) =>
//      val p1 = choreo2npom(c1)
//      val lOrder = for a <- p1.events.toSet
//                       b <- p1.events.toSet
//                       ag <- agents(p1.actions(a))
//                       bg <- agents(p1.actions(b))
//                       if ag == bg && a != b // force a!=b to drop reflexive part (inferable)
//      yield
//        (a, b)
//      NPomset(Nesting(Set(), Set(), Set(p1.events)), p1.actions, p1.pred, (mkMR(lOrder), seed))
//    case _ => sys.error(s"case not covered in chreo2npom: $c")
