package choreo.benchmarks

import caos.frontend.Configurator.Example
import caos.sos.BranchBisim
import choreo.frontend.ICECaos
import choreo.realisability.WellFormedness.*
import choreo.npomsets.{Choreo2NPom, NPomDefSOS, NPomset}
import choreo.projection.NPomDefProj
import choreo.sos.NetCausal

import scala.collection.mutable.ListBuffer

object JLAMPBench:

  val tests =
//    smallTests
    jlampTests
//    iceTests

  def smallTests = List(
    "\\mi{c_{fst}}"
      -> "\\pageref{eq:cfst}"
      -> "(c->a:r ; (a->c:y + a->c:n)) || (c->b:r ; (b->c:y + b->c:n))",
    "\\mi{c_{snd}^{strict}}"
      -> "\\pageref{eq:csnd-str}"
      -> "(((c->a:r ; (a->c:y + a->c:n)) || (c->b:r ; (b->c:y + b->c:n))) + 1) ; (c->a:t || c->b:t)",
  ).map(kv =>
    val (x, y) = choreo.DSL.restrParse(kv._2)
    (s"${kv._1._2} & ${kv._1._1}") -> (Choreo2NPom(x) + y.map(_.swap)))

  def jlampTests = List(
    "\\mi{c_{fst}}"
      -> "\\pageref{eq:cfst}"
      -> "(c->a:r ; (a->c:y + a->c:n)) || (c->b:r ; (b->c:y + b->c:n))",
    "\\mi{c_{snd}^{strict}}"
      -> "\\pageref{eq:csnd-str}"
      -> "(((c->a:r ; (a->c:y + a->c:n)) || (c->b:r ; (b->c:y + b->c:n))) + 1) ; (c->a:t || c->b:t)",
    "\\mi{c_{snd}^{lenient}}"
      -> "\\pageref{eq:csnd-len}"
      -> " ((c->a:r ; (a->c:y + a->c:n) ; c->a:t) || (c->b:r ; (b->c:y + b->c:n) ; c->b:t)) + (c->a:t || c->b:t)",
    "\\mi{c_1}"
      -> "\\pageref{ex:partialterm}"
      -> "(a->b:x + a->c:x) ; (d->b:x + d->e:x)",
    "\\mi{R_c}"
      -> "\\pageref{fig:bp-chor-example}"
      -> "a->b:x ; (b->c:x + b->d:x) ; c->d:x",
    "\\mi{R_d}"
      -> "\\pageref{fig:bp-chor-example}"
      -> "((a->b:x ; (b->a:x + b->d:x)) + (a->c:x ; (c->a:x + c->d:x))) ; d->a:x",
    "\\mi{R_e}"
      -> "\\pageref{fig:bp-dv}"
      -> "a->b:v || b->a:v\n[1->4,3->2]",
    "\\mi{R_f}"
      -> "\\pageref{fig:example-realisability}"
      -> "(a->b:yes||b->a:yes) +\n(a->b:no||b->a:no)",
    "\\mi{R_g}"
      -> "\\pageref{fig:example-realisability}"
      -> "a->b:int;\n((b->a:yes + b->a:no)\n ||\n a->b:bool)",
    "\\mi{R_h}"
      -> "\\pageref{fig:example-realisability}"
      -> "a->b:int; || a->b:bool\n[1->3]",
    "\\mi{R_i}"
      -> "\\pageref{fig:example-realisability}"
      -> "(a->b:yes + a->b:no);\na->b:int",
    "\\mi{R_i} tree-like"
      -> "\\pageref{ex:well-formed}"
      -> "(a->b:yes;a->b:int) +\n(a->b:no; a->b:int)",
    "2-buyers"
      -> "\\pageref{fig:examples-mpst}"
      -> "b1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n(b2->s:ok;b2->s:string;s->b2:date + b2->s:quit)",
    "streaming"
      -> "\\pageref{fig:examples-mpst}"
      -> "(d->r:bool||k->r:bool);\nr->c:bool;\n(d->r:bool||k->r:bool);\nr->c:bool",
    "2-buyers ill"
      -> "\\pageref{ex:bs-ill}"
      -> "b1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n((b2->s:ok||b2->s:string);s->b2:date + b2->s:quit)",
    "streaming ill"
      -> "\\pageref{ex:stream-ill}"
      -> "((d->r:bool||k->r:bool);\n r->c:bool)\n||\n((d->r:bool||k->r:bool);\n r->c:bool)",
  ).
    map(kv =>
      val (x, y) = choreo.DSL.restrParse(kv._2)
      (s"${kv._1._2} & ${kv._1._1}") -> (Choreo2NPom(x) + y.map(_.swap)))

  def iceTests = for Example(e, name, _) <- ICECaos.examples yield
    val (x, y) = choreo.DSL.restrParse(e)
    name -> (Choreo2NPom(x) + y.map(_.swap))


  type Run = (Boolean,Long)
  def runWF(p: NPomset): (Run,Run,Run,Run,Run) =
    val t1 = System.nanoTime()
    val wb = wellBranched(p)
    val t2 = System.nanoTime()
    val wc = wellChanneled(p)
    val t3 = System.nanoTime()
    val tl = treeLike(p)
    val t4 = System.nanoTime()
    val ch = choreographic(p)
    val t5 = System.nanoTime()
    val ok = wb.isEmpty && wc.isEmpty && tl.isEmpty && ch.isEmpty
    (wb.isEmpty -> ((t2-t1)/1000),
      wc.isEmpty -> ((t3-t2)/1000),
      tl.isEmpty -> ((t4-t3)/1000),
      ch.isEmpty -> ((t5-t4)/1000),
      ok -> ((t5-t1)/1000))

  def runBisim(p:NPomset, stopAt:Int=5000): Run =
    val local = NetCausal.mkInit(NPomDefProj.allProj(p).toList)
    val lSOS = NetCausal.sos(NPomDefSOS)
    val t = System.nanoTime()
    val bisim = BranchBisim.findBisim(p, local)(using NPomDefSOS, lSOS, stopAt)
    val t2 = (System.nanoTime() - t)/1000
    bisim match
      case Left(BranchBisim.BEvid(msg,_,1000)) => false -> -1
      case x => x.isRight -> t2


  def main(args:Array[String]) =
    implicit val res = ListBuffer("Example, Well-branched, Well-channeled, Tree-like, Choreographic, Well-formed, Bisimulation")
//    for e <- tests do println(s"- ${runWF(e)}")
    // Warmup caches
    for _ <- 1 to 50 do runWF(tests.head._2)
    // Run tests
    for (name,ch) <- tests do
      runCh(name,ch)
//      val (t1,t2,t3,t4) = runWF(ch)
//      val t5 = t1+t2+t3+t4
//      val t6 = runBisim(ch)
//      add(List(name,t1,t2,t3,t4,t5,t6))


  private def runCh(name:String,ch:NPomset)(using ls:ListBuffer[String]): Unit =
    // run WF
    val wf = for _ <- 1 to 10 yield runWF(ch).toList
    // compile average and error (no variance)
    val wft = wf.transpose // list/tuple with 5 lists of measurements
    val avgs = wft.map(col => col.map(_._2).sum/col.size)
    val dev = wft.zip(avgs).map((col,avg) => col.map(v=>Math.abs(v._2-avg)).sum/col.size)
    println(s"----\nvals: ${wft.map(col => col.map(_._2))}\nagvs: ${avgs}\ndev: ${dev}\n----")
    val max = wft.map(col => col.map(_._2).max)
    val min = wft.map(col => col.map(_._2).min)
    val err = avgs.zip(max).zip(min).zipWithIndex.map(x =>
      if x._2 != 4
      then x._1._1._1.toString
      else s"${x._1._1._1}$$\\pm$$${List((x._1._2-x._1._1._1).abs,(x._1._1._2-x._1._1._1).abs).max}$$\\stdv$$${x._1._2}")
    val withDev = avgs.zip(dev).zipWithIndex.map(x =>
      if x._2 != 4
      then x._1._1.toString
      else s"${x._1._1}$$\\stdv$$${x._1._2}")
    val ok = wf.head.map(_._1)
//    val merged = ok.zip(err).map(runpp)
    val merged = ok.zip(withDev).map(runpp)
//    add(name::merged.toList)
    // run bisim once
    val bi = runBisim(ch)
    if bi._2 == -1 then
      return add(name::merged.toList:::List(runpp(false->"timeout"), "-") )
    // run bisim more times if did not timeout
    val bs = bi::(for _ <- 1 to 9 yield runBisim(ch)).toList
    // compile average and error (no variance)
    val avg2 = bs.map(_._2).sum / bs.size
    val dev2 = bs.map(v=>Math.abs(v._2-avg2)).sum / bs.size
    val max2 = bs.map(_._2).max
    val min2 = bs.map(_._2).min
    val err2 = s"$avg2±${List((max2-avg2).abs, (min2-avg2).abs).max}$$\\stdv$$$dev2"
    val ok2 = bi._1
    val merged2 = runpp(ok2,err2)
    val improve = ((avgs.toList(4).toFloat / avg2.toFloat)*10000).toInt.toFloat/100
      add(name::merged.toList:::List(merged2, improve+"\\%"))


  private def add(s:Iterable[Any])(using ls:ListBuffer[String]): Unit =
    val res = s.mkString(" & ")
    println("\\\\"+res)
    ls += "\\\\\n"
    ls += res

  private def runpp(r:(Boolean,Any)): String =
    if r._1 then s"\\ok{${r._2}}" else s"\\fail{${r._2}}"


  // to print all the data as a spreadsheet
  def runAllToCSVpp: Unit =
    // Warmup caches
    for _ <- 1 to 50 do runWF(tests.head._2)
    // now do the actual work and print the result
    runAllToCSV.map(println)

  def runAllToCSV: List[String] =
    "Run nb., Example, Well-branched, wbOK?, Well-channeled, wcOK?, Tree-like, tlOK?, Choreographic, chOK?, Well-formed, wfOK?, Bisimulation, bsOK?"::
    (for (name,ch) <- tests; i <- 1 to 10 yield
      if i==1 then print(name+s": $i,")
      else if i==10 then println(i)
      else print(s"$i,")
      s"$i, $name, ${runToCSV(name,ch,i)}")
//      .sorted

  def runToCSV(name: String, ch: NPomset,nb: Int): String =
    (runWF(ch).productIterator.toList ::: List(runBisim(ch)))
      .map{
          case (b,i:Long) => s"$i, ${b.toString.toUpperCase()}"
          case _ => -2}
      .mkString(", ")



