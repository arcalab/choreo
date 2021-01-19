package choreo.choreo2

import choreo.choreo2.syntax._
import choreo.choreo2.DSL._
import choreo.choreo2.analysis.SOS._
import choreo.choreo2.syntax.Choreo.Loop

object Examples: 
  
  val a: Agent = Agent("a")
  val b: Agent = Agent("b")
  val c: Agent = Agent("c")
  val d: Agent = Agent("d")
  val e: Agent = Agent("e")
  val x: Msg = Msg(List("x"))
  val y: Msg = Msg(List("y"))
  val z: Msg = Msg(List("z"))
  val m: Msg = Msg(List("m"))
  val ack: Msg = Msg(List("ack"))

//  val end: Choreo = DSL.end
//  val tau: Choreo = DSL.tau
//  def loop(e:Choreo): Loop = DSL.loop(e)
  def simple(e:Choreo): Choreo = backend.Simplify(e)

  // 22 possible traces, size 6 (x1) or 8 (x21).
  val ex0: Choreo = (((a->b) + ((a->c) || (c->b))) > (b->d)) > (b->a) // not realsb: c can read or write
  // with pomsets it would be
  // a->b + (a->c || c->b) >  b->d  >  b->a  =
  //  1. a->b  >  b->d  >  b->a  (pomset with 6 nodes)
  //  2. (a->c || c->b) >  b->d  >  b->a   (pomset with 8 nodes)

  val exl1: Choreo = ((b→a)>(b→a)) + ((b→a)>(a→c))

  val ex1: Choreo = (b?"x1"|"m1") + (c?"x2"|"m1") > (b?"x3"|"m2") > (c?"x4"|"m2") // not realsb: b,c do not know if they can receive m2.
  val ex2a: Choreo = ((a→b)+(a→c)) > (c→d) // not realsb - c!d must wait for the decision of a, but may not know about it. (Also b waits or not.)
  val ex2b: Choreo = ((a→b)+(a→c)) > (d→c) // not realsb (b and c)
  val ex2c: Choreo = ((a→b)+(a→c)) > (a->c | m) // not realsb? c (and b is not termination aware)
  val ex2d: Choreo = ((a->b->c)+(a->c)) > (c->a | m) // realsb - NOT: b waits or not...
  val ex3: Choreo = (a?b + end) > a?c // not realisable (a may wait for b)
  val ex4: Choreo = ((a->b)+(a->c)) > (a->b|"end") > (a->c|"end") // realsb (if order preserving)
  val ex5: Choreo = (a->b->c + end) > (a->c|"end") > (a->b|"end") // not realsb (c waits for b?)
  val ex6: Choreo = (c->a) > (a->c || (b->a)) // incorrectly flagged as unrealisable... (fixed)
  val ex7: Choreo = a->c || (b->a) || (d->a) // may generate too many cases (unrealisable)
  val ex8: Choreo = a->c + (b->c) // not realsb (bad !-leader)
  val ex9a: Choreo = a->b > ((b->a|"go") + (b->a > (a->b|"stop"))) // dist1 - realisable
  val ex9b: Choreo = (a->b > (b->a|"go")) + (a->b > (b->a) > (a->b|"stop")) // dist2 - realisable
  val ex9c: Choreo = ((a->b|x) > (a->b|y)) + ((a->b|x) > (a->b|"z"))
  val ex9d: Choreo = ((a->b|x) > ( (a->b|y)) + (a->b|"z") )
  val ex10: Choreo =  ((a->b|x)>(c->b|x)) + ((a->b|y)>(c->b|y)) // bad dependency between senders
  val ex10b: Choreo = ((a->b|x)>(c->b|y)) + ((a->b|y)>(c->b|y)) // OK (KO with bisim!)
  val ex11: Choreo =  ((a->b->d)>(c->b|x)) + ((a->d->b)>(c->b|y)) // bad dependency between senders
  val ex12: Choreo =  ((a->b)>(c->d)) + ((a->d)>(c->b)) // bad dependency between senders
  val ex13a: Choreo =  (a->b|m) > ((a->b|x) + (a->b|y))
  val ex13b: Choreo = ((a->b|m) > (a->b|x)) + ((a->b|m) > (a->b|y))
  val ex14: Choreo = loop((a->b|"req") > (b->c|"look") > (b->a|"wait") > (((c->b|"ok")
    > (b->a|"rok")) + ((c->b|"ko")  > (b->a|"rko")))) // bounded
  val ex15:Choreo =  loop((a->b|"req") > (b->c|"look") > ((c->a|"ok") + (c->a|"ko"))) // bounded
  val ex16:Choreo =  loop(ex10b) // notbounded
  val ex17:Choreo = ((a->b|x) > (b->a|y)) || ((a->b|x) > (b->a|"z"))  //well thread
  val ex18:Choreo = ((a->b|x) > (a->b|x) > (b->a|y)) || ((a->b|x) > (b->a|"z")) // bad thread
  val ex19:Choreo = ((a->b|m) > (a->b|y)) || ((a->b|x) > (a->b|y))  //well thread
  val ex20:Choreo = ((a->c|"m1") > (a->c|y) > (a->b|"m2")) || ((a->c|"m1")> (a->c|x) > (a->b|"m2")) // should be well?
  val ex21:Choreo = ((c->a|x) > (a->b|y)) || ((c->a|m) > (a->b|y)) // bad thread, should be ok? 
  val ex22:Choreo = ((a->c|"l1")>(b->c|"l2")>(a->b|x))  ||
    ((a->c|"r1")>(b->c|"r2")>(a->b|x))     // bad (variation of g12)
  val ex23:Choreo = ((a->c|"l1")>(b->c|"m")>(a->b|x))  ||
    ((a->c|"r1")>(b->c|"m")>(a->b|x))     // good but currently detected as bad.
  val ex24:Choreo = ((a->c|"l1")>(b->c|"m")>(a->b|x)>(b->c|"l3"))  ||
    ((a->c|"r1")>(b->c|"m")>(a->b|x)>(b->c|"r3"))
  // bad (same as 23 but with info after shared message)
  val ex25:Choreo = ((a->c|"m1") > (d->"f"|"y") > ("f"->b|"m3") > (a->b|"x")) ||
    ((a->c|"m2") > (d->"f"|"x") > ("f"->b|"m4") > (a->b|"x"))
  val ex26:Choreo = ((a->b|x) > (b->a|y) > (b->c|m)) || ((a->b|x) > (b->a|"z")> (b->c|y))



  // Examples from Emílio's journal paper
  val g4:  Choreo = (a->b|x) + (a->b|y)
  val g6:  Choreo = ((a->b|x)>(b->c|"z")) + ((a->c|y)>(c->b|"w"))
  val g7:  Choreo = (a->b|x) + (a->c|x)
  val g8:  Choreo = (a->b|x) > (end + ((a->b|y)>(b->c|"z")))
  val g8a: Choreo = (a->b|x) > (end + (a->b|y))
  val g9:  Choreo = (a->b|x) + (c->d|x)
  val g10: Choreo = (((a->b|x)||(c->b|x)) > (a->b|"z"))  +  ((a->b|y)>(c->b|y)>(a->b|"z"))
  val g11: Choreo = (((a->b|x)>(d->b|y)) || ((a->c|x)>(d->c|y))) +
    (((d->b|y)>(a->b|x)) || ((d->c|y)>(a->c|x)))
  val g12: Choreo = ((a->c|"l1")>(b->c|"l2")>(a->b|x)>(b->c|"l3"))  ||
    ((a->c|"r1")>(b->c|"r2")>(a->b|x)>(b->c|"r3"))

  // Larger example from Emílio's paper
  val g0: Choreo = end
  val g1: Choreo = end
  val g2: Choreo = end
  // 442675 possible traces (fast to compute)
  // Example from Emílio's (journal) paper - it feels unrealisable:
  //   c->a:quit/checkBalance/widthdraw can go even if "b->a:granted" did not go yet.
  val atm: Choreo =
  (c->a|"auth") > (a->b|"authReq") > (
    ((b->a|"denied") > (a->c|"authFailed")) + (
      (b->a|"granted") > (
        (c->a|"quit") + (
          (c->a|"checkBalance") > (
            (a->c|"advert") > g0 || (
              (a->c|"advert") > g1 || (
                (b->a|"getBalance")  > g2
                )
              ) > (a->c|"balance")
            )
          ) /*quit+check*/ + (
          (c->a|"withdraw") > (a->b|"authWithdrawal") > (
            ((b->a|"allow") > (a->c|"money")) +
              ((b->a|"deny") > (a->c|"bye"))
            )
          )
        )
      )
    )
  val atm1: Choreo = nextChoreo(atm).head._2
  val atm2: Choreo = nextChoreo(atm1).head._2
  val atm3a: Choreo = nextChoreo(atm2).head._2 // only this makes sense
  val atm3b: Choreo = nextChoreo(atm2).apply(1)._2
  val atm3c: Choreo = nextChoreo(atm2).apply(2)._2
  val atm3d: Choreo = nextChoreo(atm2).apply(3)._2
  val atm4a: Choreo = nextChoreo(atm3a).head._2 // only this makes sense
  val atm5a: Choreo = nextChoreo(atm4a).head._2 // only these 2 make sense
  val atm5b: Choreo = nextChoreo(atm4a).apply(1)._2 // only these 2 make sense
  val atm6ab: Choreo = nextChoreo(atm5b).head._2 // only this makes sense

  val subatm: Choreo = (c->a|"quit") + ( (c->a|"check") > (b->a|"getBal"))

  val atmFromChorgram: Choreo = (c->a|"auth") >
    (a->b|"authReq") >
    (
      ((b->a|"denied") >
        (a->c|"authFail"))
        +
        (b->a|"granted") >
        (
          (c->a|"withdraw") >
            (a->b|"authWithdrawal") >
            (
              ((b->a|"allow") >
                (a->c|"money"))
                +
                ((b->a|"deny") >
                  (a->c|"bye"))
              )
              +
              ((c->a|"checkBalance") >
                (a->b|"getBalance") >
                (b->a|"balance") >
                (a->c|"balance"))
              +
              ((c->a|"quit") >
                (a->b|"quit"))
          )
      )

