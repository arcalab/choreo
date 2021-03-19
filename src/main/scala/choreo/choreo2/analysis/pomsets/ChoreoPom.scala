package choreo.choreo2.analysis.pomsets

//import cats.{Always, Eval, Later, Now}
//import cats.data.State
//import cats.implicits._
import choreo.choreo2.syntax.{Agent, Choreo, Msg}
import choreo.choreo2.syntax.Choreo._
import choreo.choreo2.analysis.Global
import choreo.choreo2.analysis.Global.{group, nextChoreo}
import choreo.choreo2.analysis.given_LTS_Choreo
import choreo.choreo2.analysis.pomsets._
import choreo.choreo2.analysis.pomsets.Pomset._
import choreo.choreo2.analysis.pomsets.Label._
import choreo.choreo2.analysis.pomsets.GlobalPom.globalPom

object ChoreoPom:

  private var seed:Int = 0
  private def next():Int = {seed+=1; seed-1}

  def apply(c:Choreo):Pomset = c match
    case Send(as, bs, m) =>
      val ps = as.flatMap(a => bs.map(b => send(a, b, m)))
      val p = ps.foldRight(identity)(_>>_)
      mkPomset(p)
    case Seq(c1, c2) => mkPomset(apply(c1) >> apply(c2))
    case Par(c1, c2) => mkPomset(apply(c1) * apply(c2))
    case Choice(c1, c2) => mkPomset(apply(c1) + apply(c2))
    case d@DChoice(c1, c2) =>
      //dchoice2PomViaChor(d)
      dchoice2PomViaPom(d)
    case Loop(c) =>
      val pc =  apply(c)
      mkPomset(identity + (pc >> Pomset(pc.events,pc.labels,pc.order,true)))
    case End => identity
    case act@In(b,a,m):Action =>
      val e = next()
      Pomset(Set(e),Map(e->LAct(act)),Set())
    case act@Out(a,b,m): Action =>
      val e = next()
      Pomset(Set(e),Map(e->LAct(act)),Set())
    case Tau:Action => identity //todo: check

  private def dchoice2PomViaChor(d:DChoice):Pomset =
    val next:Set[(Action,Choreo)] = d.trans
    val nextPoms:Set[Pomset] = next.map(n=>dchoice(n._1,n._2))
    var p:Pomset = identity
    if (nextPoms.size == 1) 
      then p = nextPoms.head
      else p = mkPomset(bigChoice(nextPoms))
    if d.accepting then p = p + Pomset.identity
    mkPomset(p)
  
  private def dchoice(by:Action,to:Choreo):Pomset =
    val p =  apply(to)
    mkPomset(LAct(by)->p)
  
  private def send(from:Agent,to:Agent,m:Msg):Pomset =
    val e1 = next()
    val e2 = next()
    Pomset(Set(e1,e2),Map(e1->LAct(Out(from,to,m)),(e2)->LAct(In(to,from,m))),Set(Order(e1,e2)))

  private def mkPomset(p:Pomset):Pomset =
    if p.events.nonEmpty then 
      seed = p.events.max.max(seed)+1
      p
    else p
  
  ///////////////////////////////////////////////////////////////////////
  // Danger zone: Experiments
  //////////////////////////////////////////////////////////////////////
    
  private def dchoice2PomViaPom(d:DChoice):Pomset =
    val p1:Pomset = apply(d.c1) 
    val p2:Pomset = apply(d.c2)
    val np1 = p1.trans.toList
    val np2 = p2.trans.toList
    // find common next actions 
    val ja = np1.map(_._1).intersect(np2.map(_._1))
    // for each joined action joined possible next steps 
    val nj = ja.flatMap(a => group(a,np1,np2))
    // get which action are individual to each pomset
    val ns1 = np1.filterNot(a => ja.contains(a._1))
    val ns2 = np2.filterNot(a=> ja.contains(a._1))
    // generate next possible pomsets
    var nextPoms:List[Pomset] = List()
    if ja.nonEmpty  then nextPoms = nj.map(j => dchoiceP(p1,p2,j._1,j._2,j._3))
    if ns1.nonEmpty then nextPoms ++:= ns1.map(n => dchoicePAlone(p1,n._1,n._2))
    if ns2.nonEmpty then nextPoms ++:= ns2.map(n => dchoicePAlone(p2,n._1,n._2))
    val e = next()
    var p:Pomset = Pomset.identity
    if (nextPoms.size == 1) 
      then p = nextPoms.head 
      else p = mkPomset(bigChoice(nextPoms.toSet))
      //else p = Pomset(nextPoms.flatMap(_.events).toSet+e, 
      //  nextPoms.flatMap(_.labels).toMap+(e->LPoms(nextPoms.toSet)), 
      //  nextPoms.flatMap(_.order).toSet++nextPoms.flatMap(_.events).map(e1=>Order(e,e1)).toSet+Order(e,e))
    if p1.accepting || p2.accepting then p = p + Pomset.identity
    mkPomset(p)
  
  private def dchoiceP(p1:Pomset, p2:Pomset, a:Action, toP1:Pomset, toP2:Pomset):Pomset =
    val r1 = mkPomset(diff(toP1,p1).fresh(next()))
    val r2 = mkPomset(diff(toP2,p2).fresh(next()))
    val r = mkPomset(r1 + r2)
    mkPomset(LAct(a)->r)  
    //val e = next()
    //Pomset(r.events+e, r.labels+(e->LAct(a)), r.order++r.events.map(e1=>Order(e,e1)))
    
   
  private def dchoicePAlone(from:Pomset,a:Action,to:Pomset):Pomset =
    val r1 = mkPomset(diff(to,from).fresh(next()))
    val r = mkPomset(r1.fresh(next()))
    mkPomset(LAct(a)->r)
    //val e1 = next()
    //val e2 = next()
    //Pomset(r.events++Set(e1,e2)
    //  , r.labels++Map(e1->LAct(a),e2->LPoms(Set(r)))
    //  , r.order++r.events.map(e3=>Order(e2,e3))+Order(e1,e2))
  
  private def diff(pp1:Pomset,pp2:Pomset):Pomset = 
    val p1 = pp1.transitiveClosure
    val p2 = pp2.transitiveClosure
    val e = p1.events.filter(e => !p2.events.contains(e) || (p1.labels(e) == p2.labels(e)))
    val l = p1.labels.filter(l=> e.contains(l._1)).toMap
    val o = p1.order.filter(or => e.contains(or.left)  && e.contains(or.right))
    Pomset(e,l,o,p1.loop)

  def group(a:Action,nc1:List[(Action,Pomset)],nc2:List[(Action,Pomset)]):List[(Action,Pomset,Pomset)] =
    var joinedSteps:List[(Action,Pomset,Pomset)] = List()
    for ((a1,c1) <- nc1; (a2,c2) <- nc2; if a1 == a && a2 == a) do
      joinedSteps +:= ((a1,c1,c2))
    joinedSteps