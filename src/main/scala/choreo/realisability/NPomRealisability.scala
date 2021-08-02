//package choreo.realisability
//
//import choreo.npomsets.NPomset
//import choreo.npomsets.NPomset._
//import choreo.realisability.Topology
//import choreo.realisability.Topology._
///**
// * Created by guillecledou on 01/07/2021
// *
// * EXPERMIENTS
// */
//
//object NPomRealisability:
//  def apply(p:NPomset):Boolean =
//    val proj = p.projectAll.map(p=>p.simplified)
//    val ic = p.interclosure
//    val localOrder = add(ic._2,proj.map(p=>p.pred).foldRight[Order](Map())({case (a,n) => add(n,a)}))
//    p.wellBranched && proj.forall(p=>p.wellBranched)
//      && realisable(p.pred,localOrder,p.actions)
//      //mkTopology(p).equivalent(mkTopology(proj))
//
//  /**
//   * Realisability of two partial orders over events
//   * @param go global order
//   * @param lo local order
//   * @param actions labels of events
//   * @return
//   */
//  protected def realisable(go:Order,
//                           lo:Order, actions:Actions):Boolean =
//    val invertGo = invert(go)
//    val invertLo = invert(lo)
//    // initial events in global and local orders
//    val goInit:Set[Event] = go.filter(kv=>kv._2.isEmpty).keySet ++ (actions.keySet -- go.keySet)
//    val loInit:Set[Event] = lo.filter(kv=>kv._2.isEmpty).keySet ++ (actions.keySet -- lo.keySet)
//    // ongoing experiments with some notion of history
//    //val goTail:Set[Event] = invertGo.filter(kv=>kv._2.isEmpty).keySet ++ (actions.keySet -- invertGo.keySet)
//    //val loTail:Set[Event] = invertLo.filter(kv=>kv._2.isEmpty).keySet ++ (actions.keySet -- invertLo.keySet)
//    //println(s"[realisable] - goTail: ${goTail}")
//    //println(s"[realisable] - loTail: ${loTail}")
//    goInit == loInit
//      && goInit.forall(e=>isomorphic(future(e,invertGo),future(e,invertLo),actions))
//      //&& goTail == loTail
//      //&& goTail.forall(e=>isomorphic(history(e,go),history(e,lo),actions))
//
//
//  ////
//  ////protected def futureCauses(e:Event,t:Topology[Event]):Topology[Event] =
//  ////  val st.succClosure
//
//  /**
//   * Some notion of isomorphism between partial orders over events
//   * Right now, same if they are isomorphic with respect to labels
//   * @param go global order
//   * @param lo local order
//   * @param actions label of events
//   * @return
//   */
//  protected def isomorphic(go:Order,lo:Order,actions:Actions):Boolean = //(go:Set[(Event,Event)],lo:Set[(Event,Event)],actions:Actions):Boolean =
//    //println(s"[isomorphic] - actions keys: ${actions.keySet}")
//    val geClosure = NPomset.closure(go,actions.keySet)
//    val leClosure = NPomset.closure(lo,actions.keySet)
//    val ge = geClosure.map(kv=> kv._2.map(v=>(kv._1,v))).flatten.toSet.map({case (f,t) => (actions(f),actions(t))})
//    val le = leClosure.map(kv=> kv._2.map(v=>(kv._1,v))).flatten.toSet.map({case (f,t) => (actions(f),actions(t))})
//    //println(s"[isomorphic] - global-closure: ${geClosure}")
//    //println(s"[isomorphic] - local-closure: ${leClosure}")
//    ge == le
//
//  protected def future(e:Event,succ:Order):Order =// Set[(Event,Event)] =
//    //println(s"[future] - e = $e, succ: ${succ} ")
//    val succe = NPomset.subTree(e,succ)
//    //println(s"[future] - succe: ${succe} ")
//    succe
//    //val pairs = succe.map(kv=> kv._2.map(v=>(kv._1,v))).flatten.toSet
//    //println(s"[future] - pairs = $pairs")
//    //pairs
//
//  protected def history(e:Event,pred:Order):Order =
//    //println(s"[history] - e = $e, pred: ${pred} ")
//    val prede = NPomset.subTree(e,pred)
//    //println(s"[history] - prede: ${prede} ")
//    prede
