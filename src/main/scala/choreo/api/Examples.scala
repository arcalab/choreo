package choreo.api

import choreo.syntax.*
import caos.common.Example
import choreo.common.Simplify

object Examples:

  def simple(e:Choreo): Choreo = Simplify(e)

  val m: Agent = Agent("m")
  val w1: Agent = Agent("w1")
  val w2: Agent = Agent("w2")
  val w3: Agent = Agent("w3")
  val b: Agent = Agent("b")
  val s: Agent = Agent("s")
  val price: Msg = Msg(List("price"))
  val descr: Msg = Msg(List("descr"))
  val acc: Msg = Msg(List("acc"))
  val rej: Msg = Msg(List("rej"))
  val ack: Msg = Msg(List("ack"))
  val work: Msg = Msg(List("work"))
  val done: Msg = Msg(List("done"))

  // Example 3 in paper
  val buyerSeller:Choreo =
    ((s->b|descr) || (s->b|price)) > ((b->s|acc) + (b->s|rej))

  // Example 4 in paper
  val masterWorker2:Choreo =
    (m->w1|work) > (m->w2|work) > ((w1->m|done) || (w2->m|done))

  // Example 12 in paper
  val masterWorker3:Choreo =
    (m->w1|work) > (m->w2|work) > (m->w3|work) >
      ((w1->m|done) || (w2->m|done) || (w3->m|done))

  val dummy = for i <- ( 1 to 10).toList yield Example("",s"ex$i","")

  val examples =
    Example(
      s"""// Buyer-Seller, Relaxed\n""" ++ simple(buyerSeller).toString,
      "Buyer-Seller, Relaxed",
      ""
    ):: Example(
        s"""// 1 Master - 2 Workers, Relaxed\n""" ++ simple(masterWorker2).toString,
        "1 Master - 2 Workers, Releaxed" ,
        ""
      ):: Example(
        s"""// 1 Master - 3 Workers, Relaxed\n""" ++ simple(masterWorker3).toString,
        "1 Master - 3 Workers, Relaxed" ,
        ""
      ):: Example("","Buyer-Seller","")
       :: Example("","1 Master - 2 Workers","")
       :: Example("", "1 Master - 3 Workers","")::dummy

  val dummyCode =
    """/
       |// Specific: Master API
       |//
       |
       |type M$Pom$Send[X, Q, E] = (X, Q, E) match
       |  case ((n, true, v2, v3, v4, v5, v6), W1, Work) => (
       |    n, false, v2, v3, v4, v5, v6
       |  )
       |  case ((n, false, true, v3, v4, v5, v6), W2, Work) => (
       |    n, false, false, v3, v4, v5, v6
       |  )
       |  case ((n, v1, false, true, v4, v5, v6), W3, Work) => (
       |    n, v1, false, false, v4, v5, v6
       |  )
       |  case Any => Error
       |
       |def M$Pom$send[X, Q, E](x: X, q: Q, e: E): M$Pom$Send[X, Q, E] =
       |  (x, q, e) match
       |    case ((n, v1: true, v2, v3, v4, v5, v6), _: W1, _: Work): (
       |            (n, true, v2, v3, v4, v5, v6),
       |            W1,
       |            Work
       |        ) =>
       |      (n, false, v2, v3, v4, v5, v6)
       |    case ((n, v1: false, v2: true, v3, v4, v5, v6), _: W2, _: Work): (
       |            (n, false, true, v3, v4, v5, v6),
       |            W2,
       |            Work
       |        ) =>
       |      (n, v1, false, v3, v4, v5, v6)
       |    case ((n, v1, v2: false, v3: true, v4, v5, v6), _: W3, _: Work): (
       |            (n, v1, false, true, v4, v5, v6),
       |            W3,
       |            Work
       |        ) =>
       |      (n, v1, v2, false, v4, v5, v6)
       |    case _: Any => Error()
       |
       |
       |""".stripMargin

