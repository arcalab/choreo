package choreo.view

import choreo.pomsets.Label._
import choreo.pomsets.Pomset._
import choreo.pomsets.{PomKeepSOS, _}
import choreo.syntax.Agent
import choreo.syntax.Choreo.{Action, In, Out}


/**
 * Created by guillecledou on 31/10/2020
 */


/** Visualisation of pomsets using Graphviz's Dot language. */
object DotPomsets:
  private var seedId:Int = 0 // todo: change to a State modand
  private def seed():Int = {seedId+=1;seedId-1} 
   given dotPomset:Dot[Pomset] with
      extension(p:Pomset)
        def toDot: String =
          //seedId = p.allEvents.max+1
          seedId = if p.events.nonEmpty then p.events.max+1 else 0 
          s"""
             |digraph G {
             |rankdir = "LR";
             |compound=true;
             | ${DotPomsets.mkCluster(p.reduce)(seed())}
             |}
             |""".stripMargin

  //given dotPomLTS as Dot[Pomset]:
  //    extension (p:Pomset)
  //      def toDot:String = dotPomLTS(p)
  //
  //    def dotPomLTS(pom:Pomset):String =
  //      val p = pom.reduce
  //      seedId = p.events.max+1
  //      val trans:Set[(Action,Pomset)] = p.trans
  //      //val poms = trans.map(t=>t._2).toSet.zipWithIndex ++ 
  //      //val fromPom = (p,toPoms.size+1)
  //      val transId = trans.map(t=>(t._1,t._2,seed()))
  //      val fromId = seed()
  //      //val poms = (trans.map(t=>t._2) + p).map(pom=>(pom,seed()))
  //      //val pomsMap = poms.toMap
  //      //val transId = trans.map(t=>(t._1,t._2,pomsMap(t._2)))
  //      s"""
  //         |digraph G {
  //         |rankdir = "LR";
  //         |compound=true;
  //         | ${mkCluster(p)(fromId)} 
  //         | ${transId.map(t=>mkTran(fromId,t)).mkString("\n")}
  //         |}
  //         |""".stripMargin
  //                      
  //    def mkTran(from:Int,t:(Action,Pomset,Int)):String =
  //      s"""
  //         | ${mkCluster(t._2)(t._3)}
  //         | $from -> ${t._3} [label="${t._1.toString}",color=gold]
  //         |""".stripMargin
           
      //def mkTrans(from:Int,to:Int,by:Action):String =
      //  s"""$from -> $to [label="${by.toString}",color=gold]"""

  def mkCluster(p:Pomset)(implicit id:Int):String =
    s"""
       |subgraph cluster_P${id} {
       | ${id} [label="",peripheries=0,height=0,width=0,style=invis];
       | ${if p.loop then "color=orange;label=Loop;" else """color=black;label="";""" }
       | ${mkRanks(p)}
       | ${p.labels.filter(l=>p.uniqueEvents.contains(l._1)).map(l=>mkLabel(l,p)).mkString("\n  ")}
       | ${p.order.filter(o=>p.uniqueOrders.contains(o)).map(o=>mkOrder(o,p.labels)).mkString("\n  ")}
       |}
       |""".stripMargin
  
  def mkRanks(p:Pomset):String =  ""
    //val uniqueAEvent = p.agents.map(a=> p.eventsOf(a).filter(p.uniqueEvents.contains(_)))
    //val eventsPerA = uniqueAEvent.map(a=>a.filter(e=>p.labels(e).simple))
    //eventsPerA.map(es=> s"""{rank=same; ${es.mkString(";")}}""" ).mkString("\n")
  
  def mkLabel(l:(Event,Label),pom:Pomset):String  = l._2 match
    case LAct(In(a, b, m)) => s"""${l._1} [label="${b.s}?${a.s}${m.pp}"]; """
    case LAct(Out(a, b, m))=> s"""${l._1} [label="${a.s}!${b.s}${m.pp}"]; """
    case LPoms(ps) =>
      val pid:Set[(Int,Pomset)] = ps.map(p=> (seed(),p))
      pid.map(p=>mkCluster(p._2)(p._1)).mkString("\n") +
        pid.map(p=>mkOrder(Order(l._1,p._1),pom.labels)).mkString("\n")
    case _ => "" // tau to avoid warnings
  
  def mkOrder(o:Order,labels:Labels):String = (labels.get(o.left),labels.get(o.right)) match
    case (Some(ll),Some(lr)) if ll.simple && ll.simple && ll.actives.intersect(lr.actives).nonEmpty =>
      //trick to make agent block align from top to bottom  
      //s"""${o.right} -> ${o.left} [color="black" dir=back];"""
      s"""${o.left} -> ${o.right} [color="red"];"""
    case (Some(ll),Some(lr)) =>
      s"""${o.left} -> ${o.right} [color="black"];"""
    case _ =>
      s"""${o.left} -> ${o.right} [color="blue", lhead=cluster_P${o.right}];"""
  