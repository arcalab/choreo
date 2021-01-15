package choreo.choreo2.analysis

import choreo.choreo2.syntax._
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.syntax.Agent._

import scala.sys.error

object Lucs :
  /////////////////////////
  /// Luc's experiments ///
  /////////////////////////

  def compat(c1:Choreo,c2:Choreo): Boolean =
    c1==c2 || distp(c1,c2)

  def dista(c1:Choreo,c2:Choreo): Boolean = (c1,c2) match
    case (Send(as, bs, m),_) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      dista(cs.fold[Choreo](End)(_>_),c2)
    case (_,Send(as, bs, m)) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      dista(c2,cs.fold[Choreo](End)(_>_))

    case (Seq(c1a, c1b),Seq(c2a,c2b)) if c1a==c2a => dista(c1b,c2b)
    case (Seq(c1a,_),Seq(c2a,_)) => dista(c1a,c2a)
    case (Seq(c1a,_),_) => dista(c1a,c2)
    case (_,Seq(c2a,_)) => dista(c1,c2a)
    case (Par(_,_),_) => error("[dista] parallel not handled")
    case (_,Par(_,_)) => error("[dista] parallel not handled")
    case (Choice(c1a, c1b),_) => dista(c1a,c2) || dista(c1b,c2)
    case (_,Choice(c2a, c2b)) => dista(c1,c2a) || dista(c1,c2b)
    case (Loop(_),_) => error("[dista] loops nos handled")
    case (_,Loop(_)) => error("[dista] loops nos handled")
    case (Out(a,_,_),Out(b,_,_)) => a==b
    case (Out(_,_,_),_) => false
    case (_,Out(_,_,_)) => false
    case (In(_,_,_),_) => false
    case (_,In(_,_,_)) => false
    case (End,End) => false
    case (End,_) => true // ?
    case (_,End) => true // ?

  def distp(c1:Choreo, c2:Choreo): Boolean = (c1,c2) match
    case (Send(as, bs, m),_) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      distp(cs.fold[Choreo](End)(_>_),c2)
    case (_,Send(as, bs, m)) =>
      val cs = for a<-as; b<-bs yield Out(a,b,m)>In(b,a,m)
      distp(c2,cs.fold[Choreo](End)(_>_))

    case (Seq(c1a, c1b),Seq(c2a,c2b)) if c1a==c2a => distp(c1b,c2b)
    case (Seq(c1a,_),Seq(c2a,_)) => distp(c1a,c2a)
    case (Seq(c1a,_),_) => distp(c1a,c2)
    case (_,Seq(c2a,_)) => distp(c1,c2a)
    case (Par(_,_),_) => error("[dista] parallel not handled")
    case (_,Par(_,_)) => error("[dista] parallel not handled")
    case (Choice(c1a, c1b),_) => distp(c1a,c2) || distp(c1b,c2)
    case (_,Choice(c2a, c2b)) => distp(c1,c2a) || distp(c1,c2b)
    case (Loop(_),_) => error("[dista] loops nos handled")
    case (_,Loop(_)) => error("[dista] loops nos handled")
    case (End,End) => false
    case (End,_) => true // ?
    case (_,End) => true // ?
    case (In(a1,b1,m1),In(a2,b2,m2)) => (a1==a2) && ((b1!=b2) || (m1!=m2))
    case (In(_,_,_),_) => false
    case (_,In(_,_,_)) => false
    case (Out(_,_,_),_) => false
    case (_,Out(_,_,_)) => false

  def compat(ag:Agent, c1:Choreo, c2:Choreo): Boolean =
    compat(proj(c1,ag),proj(c2,ag))
  def dista(ag:Agent, c1:Choreo, c2:Choreo): Boolean =
    dista(proj(c1,ag),proj(c2,ag))
  def distp(ag:Agent, c1:Choreo, c2:Choreo): Boolean =
    distp(proj(c1,ag),proj(c2,ag))


  def realisableLuc(c:Choreo): Boolean = c match
    case Send(_, _, _) => true
    case Seq(c1, c2) =>
      val r1 = realisableLuc(c1)
      val r2 = realisableLuc(c2)
      println(s"[real] '$c1'($r1) ; '$c2'($r2)")
      r1 && r2
    case Par(c1, c2) => error("[real] parallel not handled")
    case Choice(c1, c2) =>
      val r1 = realisableLuc(c1)
      val r2 = realisableLuc(c2)
      val ags = agents(c1)++agents(c2)
      val r3 = ags.forall(a => compat(a,c1,c2))
      val r4 = ags.exists(b => dista(b,c1,c2) &&
        (ags-b).forall(a => compat(a,c1,c2)))
      println(s"[real] '$c1'($r1) +[$r3,$r4] '$c2'($r2)")
      r1 && r2 &&  (r3 || r4)
    case Loop(c) => error("[real] loops not handled")
    case End => true
    case _: Action => true
