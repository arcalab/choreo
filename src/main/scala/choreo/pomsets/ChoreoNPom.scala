package choreo.pomsets

import cats.{Always, Eval, Later, Now}
import cats.data.State
import cats.implicits._
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.pomsets.NPom
import choreo.pomsets.NPom._
import choreo.syntax.Choreo._

import scala.sys.error

@deprecated
object ChoreoNPom:

  type St[A] = State[Int,A]

  def apply(c:Choreo):NPom = pomsetOf(c).runA(0).value

  private def pomsetOf(c:Choreo):St[NPom] = c match
    case Send(as, bs, m) =>
      for
      ps <- as.flatTraverse(a => bs.traverse(b => send(a, b, m)))
        yield ps.foldRight[NPom](identity)(_>>_)
    case Seq(c1, c2) =>
      for
      p1 <- pomsetOf(c1)
      p2 <- pomsetOf(c2)
        yield p1 >> p2
    case Par(c1, c2) =>
      for
      p1 <- pomsetOf(c1)
      p2 <- pomsetOf(c2)
        yield p1 * p2
    case Choice(c1, c2) =>
      for
      p1 <- pomsetOf(c1)
      p2 <- pomsetOf(c2)
        yield p1 + p2
    case Loop(c) =>
      for
        p <- pomsetOf(c)
      yield LPomset(p.events,p.labels,p.order)//(p>>LPomset(p.events,p.labels,p.order)) + identity
    case End => State.pure(identity)
    case In(b,a,m):Action => in(b,a,m)
    case Out(a,b,m): Action => out(a,b,m)
    case Tau:Action => State.pure(identity) //todo: check
    case _ => error(s"Case $c:${c.getClass.getName} not supported.")

  private def send(from:Agent,to:Agent,m:Msg):St[NPom] = for
  e <- State.get
  _ <- State.set(e+2)
    yield SPomset(Set(e,e+1),Map(e->LOut(from,to,m),(e+1)->LIn(to,from,m)),Set(Order(e,e+1)))

  private def out(from:Agent,to:Agent,m:Msg):St[NPom] = for
  e <- State.get
  _ <- State.set(e+1)
    yield SPomset(Set(e),Map(e->LOut(from,to,m)),Set())

  private def in(to:Agent,from:Agent,m:Msg):St[NPom] = for
  e <- State.get
  _ <- State.set(e+1)
    yield SPomset(Set(e),Map(e->LIn(to,from,m)),Set())