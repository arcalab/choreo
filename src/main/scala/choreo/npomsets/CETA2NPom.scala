package choreo.npomsets

import choreo.common.MRel.*
import choreo.npomsets.NPomset.*
import choreo.syntax.Choreo.*
import choreo.syntax.{Agent, Choreo, Msg}

object CETA2NPom:

  /** Converts a choreo expression into a Nested Pomset (with nested sets) */
  def apply(c: Choreo):NPomset =
    (new CETA2NPom).ceta2npom(c)

class CETA2NPom:

  private var seed:Int = 1
  private def next():Int = {seed+=1; seed-1}
  private def updSeed(p:NPomset): NPomset =
    seed = (p.events.toSet+(seed-1)).max+1
    NPomset(p.events,p.actions,p.pred,p.loop._1->seed)


  private def ceta2npom(c:Choreo):NPomset = c match
    case _: Send | _:Internal | _:Action =>
      val e = next()
      NPomset(Nesting(Set(e), Set(), Set()), Map(e -> c), Map(), (Map(), 0))

    case Seq(c1, c2)    => updSeed(ceta2npom(c1) >> ceta2npom(c2))
    case Par(c1, c2)    => updSeed(ceta2npom(c1) ++ ceta2npom(c2))
    case Choice(c1, c2) => updSeed(ceta2npom(c1) or ceta2npom(c2))
    case End => empty
    //case Tau => empty //todo: check
    case Loop(c1) =>
      val p1 = ceta2npom(c1)
      val lOrder = for  a<-p1.events.toSet
                        b<-p1.events.toSet
                        ag<-agents(p1.actions(a))
                        bg<-agents(p1.actions(b))
                        if ag==bg && a!=b // force a!=b to drop reflexive part (inferable)
      yield
        (a,b)
      NPomset(Nesting(Set(),Set(),Set(p1.events)),p1.actions,p1.pred, (mkMR(lOrder),seed))
    case _ => sys.error(s"case not covered in ceta2npom: $c")


