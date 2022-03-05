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
  val price: Msg = Msg(List("Price"))
  val descr: Msg = Msg(List("Descr"))
  val acc: Msg = Msg(List("Acc"))
  val rej: Msg = Msg(List("Rej"))
  val ack: Msg = Msg(List("Ack"))
  val work: Msg = Msg(List("Work"))
  val done: Msg = Msg(List("Done"))

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

  val dummy = for i <- ( 1 to 10).toList yield Example("",s"Ex. $i","")

  val seller =
    """def seller(s: S$Initial): S$Final = s
       |  .send(B, new Descr)
       |  .send(B, new Price)
       |  .recv(
       |    (_,_,s) => { println("offer accepted"); s },
       |    (_,_,s) => { println("offer rejected"); s }
       |  )""".stripMargin

  val sellerHtml = seller.replace("\n","<br>")

  val buyer =
    """def buyer(s: B$Initial): B$Final = s
      |  .recv(
      |    (_,_,s) => s.recv((_,_,s) => s.send(S, new Acc)),
      |    (_,_,s) => s.recv((_,_,s) => s.send(S, new Rej))
      |  )""".stripMargin

  val buyerHtml = buyer.replace("\n","<br>")

  val run =
    """val pr = new Protocol
      |
      |pr.run(master);
      |pr.run(worker1); pr.run(worker2)""".stripMargin

  val runHtml = run.replace("\n","<br>")

  val examples =
    Example(
      "// Buyer-Seller, Basic\n" +
        "s->b:Descr .\ns->b:Price .\n(s->b:Acc+s->b:Rej)",
      "Buyer-Seller, Basic",
      s"""<strong>Basic protocol for the Buyer-Seller example</strong>
         |
         |The code below is a possible implementation of a process that follows this protocol, assuming the classes Descr, Price, Acc, and Rej exist.
         |<pre><code class="language-scala">$sellerHtml<br><br>$buyer<br><br>$runHtml</code></pre>""".stripMargin
    ):: Example(
      s"""// 1 Master - 2 Workers, Basic\n""" +
        "m->w1:Work . m->w2:Work .\nw1->m:Done . w2->m:Done",
      "1Master-2Workers, Basic",
      "<strong>Master-Worker: Basic protocol</strong>"
    ):: Example(
      s"""// Buyer-Seller, Relaxed\n""" +
        "(s->b:Descr || s->b:Price) .\n(b->s:Acc + b->s:Rej)",
      "Buyer-Seller, Relaxed",
      ""
    ):: Example(
      s"""// 1 Master - 2 Workers, Relaxed\n""" +
        "m->w1:Work . m->w2:Work .\n(w1->m:Done || w2->m:Done)",
      "1Master-2Workers, Relaxed" ,
      s"""<strong>Master-Worker: relaxed protocol</strong>
         |
         |The code below is a possible implementation of a process that follows this protocol, using forks and joins.
         |<pre style="font-size: 1.1rem;">def master(s: M$$Initial): M$$Final =
         |  val (s1,s2) = s.send(W1, new Work)
         |                 .send(W2, new Work).fork()
         |  val f1 = Future{ s1.recv((_,_,s) =>
         |                     { println("#1"); s})}
         |  val f2 = Future{ s2.recv((_,_,s) =>
         |                     { println("#2"); s})}
         |  Await.result(
         |    for { t1 <- f1; t2 <- f2 } yield
         |       M$$State.join(t1, t2),
         |    Duration.Inf)
         |${" "}
         |def worker1(s: W1$$Initial): W1$$Final = s
         |  .recv((_, _, s) =>
         |    Thread.sleep(Random.nextInt(1000))
         |    s.send(M, Done())
         |)
         |def worker2(s: W2$$Initial): W2$$Final = s
         |  .recv((_, _, s) =>
         |    Thread.sleep(Random.nextInt(1000))
         |    s.send(M, Done())
         |)
         |val pr = new Protocol
         |pr.run(master)
         |pr.run(worker1)
         |pr.run(worker2)</pre>""".stripMargin
    ):: Example(
      s"""// 1 Master - 3 Workers, Relaxed\n""" +
        "m->w1:Work . m->w2:Work . m->w3:Work .\n(w1->m:Done || w2->m:Done || w3->m:Done)",
      "1Master-3Workers, Relaxed" ,
      ""
    )::dummy

