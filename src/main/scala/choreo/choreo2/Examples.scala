package choreo.choreo2

import choreo.choreo2.syntax._
import choreo.choreo2.DSL._
import choreo.choreo2.analysis.Global.nextChoreo
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
                    // (Jose) I think it is bad (based on bisim):
                    //   - Local  does a!c:l1/r2  then  a!b:x;b!c:m (both from left or right), then can do b?a:x
                    //   - Global does a!c:l1/r2  then  a!b:x;b!c:m (one from each side), then cannot do b?a:x
  val ex23a: Choreo = ((b->c) > (a->b)) || ((b->c) > (a->b))
  val ex24:Choreo = ((a->c|"l1")>(b->c|"m")>(a->b|x)>(b->c|"l3"))  ||
                    ((a->c|"r1")>(b->c|"m")>(a->b|x)>(b->c|"r3"))
  // bad (same as 23 but with info after shared message)
  val ex25:Choreo = ((a->c|"m1") > (d->"f"|"y") > ("f"->b|"m3") > (a->b|"x")) ||
    ((a->c|"m2") > (d->"f"|"x") > ("f"->b|"m4") > (a->b|"x"))
  val ex26:Choreo = ((a->b|x) > (b->a|y) > (b->c|m)) || ((a->b|x) > (b->a|"z")> (b->c|y))
  val ex27: Choreo = (((a->b|x)>(b->a|y)) + end) > (a->b|z)  // realisable - but still a bug in bisim  
  val ex28a: Choreo = ((a->b|x) + (a->c)) > (c->d) > (a->b|z)
  val ex28b: Choreo = loop((a->b|x) > (b->a|y)) > (a->b|z)



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
  
  val atmTest:Choreo =
   
    (
      ((b->a|"denied") >
        (a->c|"authFail"))
        +
        (b->a|"granted") >
        (
          (c->a|"withdraw") 
          )
      )
  
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
  
  val allList = List(
    ("exl1", exl1),
    ("ex1", ex1),
    ("ex2a", ex2a),
    ("ex2b", ex2b),
    ("ex2c", ex2c),
    ("ex2d", ex2d),
    ("ex3", ex3),
    ("ex4", ex4),
    ("ex5", ex5),
    ("ex6", ex6),
    ("ex7", ex7),
    ("ex8", ex8),
    ("ex9a", ex9a),
    ("ex9b", ex9b),
    ("ex9c", ex9c),
    ("ex9d", ex9d),
    ("ex10", ex10),
    ("ex10b", ex10b),
    ("ex11", ex11),
    ("ex12", ex12),
    ("ex13a", ex13a),
    ("ex13b", ex13b),
    ("ex14", ex14),
    ("ex15", ex15),
    ("ex16", ex16),
    ("ex17", ex17),
    ("ex18", ex18),
    ("ex19", ex19),
    ("ex20", ex20),
    ("ex21", ex21),
    ("ex22", ex22),
    ("ex23", ex23),
    ("ex23a", ex23a),
    ("ex24", ex24),
    ("ex25", ex25),
    ("ex26", ex26),
    ("ex27", ex27),
    ("ex28a", ex28a),
    ("ex28b", ex28b),
    ("g4", g4),
    ("g6", g6),
    ("g7", g7),
    ("g8", g8),
    ("g8a", g8a),
    ("g9", g9),
    ("g10", g10),
    ("g11", g11),
    ("g12", g12),
    ("g0", g0),
    ("g1", g1),
    ("g2", g2),
    ("atm", atm),
    ("atm1", atm1),
    ("atm2", atm2),
    ("atm3a", atm3a),
    ("atm3b", atm3b),
    ("atm3c", atm3c),
    ("atm3d", atm3d),
    ("atm4a", atm4a),
    ("atm5a", atm5a),
    ("atm5b", atm5b),
    ("atm6ab", atm6ab),
    ("subatm", subatm),
    ("atmFromChorgram", atmFromChorgram),
    ("featureOrBug?",atmTest)
  )
  
  val all = allList.toMap
  
  def getOkReal =
    all.filter( (a,b) =>
      choreo.choreo2.analysis.SyntAnalysis.realisable(b))
  def getOkBisim =
    all.filter( (a,b) =>
      choreo.choreo2.analysis.Bisimulation.findBisim(b).nonEmpty)
  def getOkWBisim =
    all.filter( (a,b) => {
      println(s"#### Going for $a #####")
      choreo.choreo2.analysis.Bisimulation.findWBisim(b).nonEmpty})
  def getOkWBisim2 =
    (all).filter( (a,b) => {
      println(s"#### Going for $a #####")
      choreo.choreo2.analysis.Bisimulation.findWBisim2(b).isRight})

  

  val okReal = all.view.filterKeys(Set(
    "ex6", "ex7", "ex9a", "ex9b", "ex9c", "ex9d",
    "ex10b", "ex13a", "ex13b", "ex16", "ex28b",
    "g4", "g0", "g1", "g2", "atm3b", "atm5a"
  ))

  val okBisim = all.view.filterKeys(Set(
    "ex6", "ex7", "ex9a", "ex9d", "ex13a", "ex14", "ex15", "ex21", "ex28b", "g4", "g6", "g0", "g1", "g2"
  ))

  val okWBisim = all.view.filterKeys(Set(
    "ex6", "ex7", "ex9a", "ex9d", "ex13a", "ex21", "g4", "g0", "g1", "g2"
  ))

  val okWBisim2 = all.view.filterKeys(Set(
    "ex6","ex7","ex9a","ex9d","ex13a","ex14","ex15","ex21","ex28b",
    "g4","g6","g0","g1","g2"
    //// dropping c1+c2 -tau-> 0
    //"ex2d","ex6","ex7","ex9a","ex9d","ex13a","ex14","ex15","ex21","ex27","ex28b",
    //"g4","g6","g7","g8","g8a","g0","g1","g2"
    //// also dropping netw-empty for local acceptance
    // "ex2d","ex6","ex7","ex9a","ex9d","ex13a","ex14","ex15","ex21","ex27","ex28b",
    // "g4","g6","g0","g1","g2"
    //// only dropping netw-empty - same as with normal
    // "ex6","ex7","ex9a","ex9d","ex13a","ex14","ex15","ex21","ex28b",
    // "g4","g6","g0","g1","g2"
    //// after removing redundancy in bisimulation search (just dropping local acceptance)
    // "ex6","ex7","ex9a","ex9d","ex13a","ex14","ex15","ex21","ex28b",
    // "g4","g6","g0","g1","g2"
  ))

