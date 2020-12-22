package choreo.syntax

import choreo.syntax.Choreo2.ack

import scala.language.implicitConversions
import scala.sys.error

case class Send(as:List[Agent2],bs:List[Agent2],m:Msg) extends Choreo2
case class Seq2(c1:Choreo2, c2:Choreo2) extends Choreo2
case class Par2(c1:Choreo2, c2:Choreo2) extends Choreo2
case class Choice2(c1:Choreo2, c2:Choreo2) extends Choreo2
case class Loop2(c:Choreo2) extends Choreo2
case object End extends Choreo2

object Choreo2 {
  def loop(e:Choreo2): Loop2 = Loop2(e)
  implicit def str2agent(s:String):Agent2 = Agent2(s)
  implicit def str2Msg(s:String):Msg = Msg(List(s))

  val a: Agent2 = Agent2("a")
  val b: Agent2 = Agent2("b")
  val c: Agent2 = Agent2("c")
  val d: Agent2 = Agent2("d")
  val m: Msg = Msg(List("m"))
  val n: Msg = Msg(List("n"))
  val ack: Msg = Msg(List("ack"))

  def go(c:Choreo2): Unit = {
    val nc = next(c)(Set())
    println(nc.map(p=>s" - ${p._1} ~~> ${p._2}").mkString("\n"))
  }

  def go(c:Choreo2,n:Int): Unit =
    println(goS(c,n).mkString("\n"))

  // 22 possible traces, size 6 (x1) or 8 (x21).
  val ex1 = (((a->b) + ((a->c) || (c->b))) > (b->d)) > (b->a)
  // with pomsets it would be
  // a->b + (a->c || c->b) >  b->d  >  b->a  =
  //  1. a->b  >  b->d  >  b->a  (pomset with 6 nodes)
  //  2. (a->c || c->b) >  b->d  >  b->a   (pomset with 8 nodes)

  val ex2 = (b?"x1" by "m1") + (c?"x2" by "m1") > (b?"x3" by "m2") > (c?"x4" by "m2")
  val ex3 = ((a→b)+(a→c)) > (c→d) // not realsb - c!d must wait for the decision of a, but may not know about it.
  val ex4 = ((a→b)+(a→c)) > (d→c) // not realsb
  val ex5 = ((a→b)+(a→c)) > (a→c) // realsb

  private def goS(c:Choreo2,n:Int): List[String] = n match {
    case 0 => List(c.toString)
    case _ =>
      val nc = next(c)(Set())
      nc.flatMap(p=> {
        val rec = goS(p._2,n-1)
        if (rec.isEmpty)
          List(s"${p._1} /> End")
        else
          for (s <- rec)
            yield s"${p._1} / $s"
      })
  }

  /** SOS: next step of a Choreo expression */
  def next(c:Choreo2)(implicit ignore:Set[Agent2]): List[(Action,Choreo2)] = c match {
    case Send(List(a), List(b), m) =>
      if (ignore contains a) Nil else List((a!b by m) -> (b?a by m))
    case Send(a::as, bs, m) => next(Send(List(a),bs,m) || Send(as,bs,m))
    case Send(as, b::bs, m) => next(Send(as,List(b),m) || Send(as,bs,m))
    case Seq2(c1, c2) =>
      val nc1 = next(c1)
      val a1 = agents(c1)
      val nc2 = next(c2)(ignore++a1)
      nc1.map(p=>p._1->simple(p._2>c2)) ++
      nc2.map(p=>p._1->simple(c1>p._2)) ++
        (if (c1.isInstanceOf[Loop2]) nc2 else Nil)
    case Par2(c1, c2) =>
      val nc1 = next(c1)
      val nc2 = next(c2)
      nc1.map(p => p._1 -> simple(p._2||c2)) ++
      nc2.map(p => p._1 -> simple(c1||p._2))
    case Choice2(c1, c2) =>
      val nc1 = next(c1)
      val nc2 = next(c2)
      nc1 ++ nc2
    case Loop2(c2) =>
      val nc2 = next(c2)
      nc2.map(p=>p._1 -> (p._2>c))
    case End => Nil
    case In(a, b, m) =>
      if (ignore contains a) Nil else List(In(a,b,m) -> End)
    case _ => error("Unknonwn next for $c")
  }

  private def simple(c: Choreo2): Choreo2 = c match {
    case Seq2(End, c2) => c2
    case Seq2(c1, End) => c1
    case Par2(End, c2) => c2
    case Par2(c1, End) => c1
    case _ => c
  }

  def agents(c:Choreo2): Set[Agent2] = c match {
    case Send(a, b, _) => a.toSet ++ b.toSet
    case Seq2(c1, c2) => agents(c1) ++ agents(c2)
    case Par2(c1, c2) => agents(c1) ++ agents(c2)
    case Choice2(c1, c2) => agents(c1) ++ agents(c2)
    case Loop2(c) => agents(c)
    case End => Set()
    case In(a, b, _) => Set(a,b)
  }


  /*
  def findReal(c:Choreo2): Option[...]
    - Overall idea: start with Set(c); pop

    - start with all "step->c2"
    - collect all steps that are !
      - if from different agents -> not realisable
      - if from the same agent "a"
        - collect all other steps (?)
           - if any is from the same agent -> not realisable
           -
        - store set of destinations c2
        - if set was known -> realisable!
   */
}

sealed trait Choreo2 {
  def >(e:Choreo2): Choreo2 = Seq2(this,e)
  def ||(e:Choreo2): Choreo2 = Par2(this,e)
  def +(e:Choreo2): Choreo2 = Choice2(this,e)
  def loop: Choreo2 = Loop2(this)
  def by(m:Msg):Choreo2 = this match {
    case Seq2(c1, c2) => Seq2(c1 by m,c2 by m)
    case Par2(c1, c2) => Par2(c1 by m,c2 by m)
    case Choice2(c1, c2) => Choice2(c1 by m,c2 by m)
    case Loop2(c) => Loop2(c by m)
    case In(a, b, m2) => In(a,b,m++m2)
    case Send(a, b, m2) => Send(a,b,m++m2)
    case End => End
  }

  override def toString: String = this match {
    case Send(a, b, m) => s"${a.mkString(",")}->${b.mkString(",")}${m.pp}"
    case Seq2(c1, c2) =>s"${mbP(c1)} > ${mbP(c2)}"
    case Par2(c1, c2) =>s"${mbP(c1)} || ${mbP(c2)}"
    case Choice2(c1, c2) => s"${mbP(c1)} + ${mbP(c2)}"
    case Loop2(c) => s"${mbP(c)}^*"
    case In(a,b,m) => s"$a?$b${m.pp}"
    case End => "0"
  }

  private def mbP(choreo: Choreo2): String = choreo match {
    case _:Seq2| _:Par2 | _:Choice2 => s"($choreo)"
    case _ => choreo.toString
  }
}

//case class Tag(c:Choreo2,m:Msg) extends Choreo2

case class In(a:Agent2,b:Agent2,m:Msg) extends Action with Choreo2
case class Out(a:Agent2,b:Agent2,m:Msg) extends Action {
  override def toString: String = s"$a!$b${m.pp}"
  def by(m2:Msg): Out = Out(a,b,m++m2)
}

sealed trait Action

case class Msg(l:List[String]) {
  def pp:String = if (l.isEmpty) "" else ":"+l.reverse.mkString("/")
  def +(m:String): Msg = Msg(m::l)
  def ++(m:Msg): Msg = Msg(m.l:::l)
}

case class Agent2(s:String) {
  def !(to:Agent2): Out = Out(this,to,Msg(Nil))
  def ?(from:Agent2): In = In(this,from,Msg(Nil))

  def ->(to:Agent2): Send = Send(List(this),List(to),Msg(Nil))
  def →(to:Agent2): Send = ->(to)
  def -->(b:Agent2): Choreo2 = (this->b) > ((b->this) by ack)

  override def toString: String = s
}

