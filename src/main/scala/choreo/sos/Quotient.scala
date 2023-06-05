package choreo.sos

import caos.sos.SOS

case class Quotient[GAct,LAct,St](eqs: St => Set[St],
                                  updAct: GAct => Option[LAct],
                                  sos: SOS[GAct,St]) extends SOS[LAct,Set[St]]:

  type Q = Set[St]
  type EqClasses = St => Q

  def mkInit(s:St): Q =
    eqs(s)

  def next[A >: LAct](q:Q): Set[(A,Q)] =
    q.flatMap(s =>
      sos.next(s).flatMap( (act,st) =>
        updAct(act).map(_ -> eqs(st))))

  override def accepting(s: Set[St]): Boolean =
    // println(s"Checking acceptance of ${s}.\n - ${s.map(local => s"$local->${sos.accepting(local)}").mkString("\n - ")}")
    s.exists(local => sos.accepting(local))
