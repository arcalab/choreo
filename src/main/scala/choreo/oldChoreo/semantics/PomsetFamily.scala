package choreo.oldChoreo.semantics

import choreo.oldChoreo.Agent

/**
 * Created by guillecledou on 31/10/2020
 */


case class PomsetFamily(pomsets:Set[Pomset]):

  def sequence(other:PomsetFamily):PomsetFamily =
    PomsetFamily(for p1<-pomsets; p2<-other.pomsets yield p1 >> p2)

  def >>(other:PomsetFamily):PomsetFamily = this.sequence(other)

  def parallel(other:PomsetFamily):PomsetFamily =
    PomsetFamily(for p1<-pomsets; p2<-other.pomsets yield p1 * p2)

  def ||(other:PomsetFamily):PomsetFamily = this.parallel(other)

  def choice(other:PomsetFamily):PomsetFamily =
    PomsetFamily(pomsets++other.pomsets)

  def +(other:PomsetFamily):PomsetFamily = this.choice(other)

  def loop(n:Int):PomsetFamily =
    List.fill(n)(this).foldRight(PomsetFamily.identity)(_>>_)

  def ^(n:Int):PomsetFamily = this.loop(n)

  def compose(other:PomsetFamily):PomsetFamily =
    PomsetFamily(for p1<-pomsets; p2<-other.pomsets yield p1.sync(p2))

  def project(a:Agent):PomsetFamily =
    PomsetFamily(pomsets.map(p => p.project(a)))

object PomsetFamily:
  val identity:PomsetFamily = PomsetFamily(Set(Pomset.identity))

