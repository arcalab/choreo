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


  val examples =
    Example(
      s"""// Buyer-Seller, Relaxed\n""" ++ simple(buyerSeller).toString,
      "Buyer-Seller",
      ""
    ):: Example(
        s"""// 1 Master - 2 Workers, Relaxed\n""" ++ simple(masterWorker2).toString,
        "1 Master - 2 Workers" ,
        ""
      ):: Example(
        s"""// 1 Master - 3 Workers, Relaxed\n""" ++ simple(masterWorker3).toString,
        "1 Master - 3 Workers" ,
        ""
      )::Nil


