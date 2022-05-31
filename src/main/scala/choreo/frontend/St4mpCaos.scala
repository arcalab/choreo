package choreo.frontend

import caos.frontend.Configurator
import Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.view.{Mermaid, Text}
import choreo.api.Session
import choreo.frontend.APICaos.chor2npom
import choreo.npomsets.Choreo2NPom
import choreo.projection.{ChorDefProj, ChorNoTauProj}
import choreo.sos.ChorDefSOS
import choreo.syntax.Choreo.agents
import choreo.syntax.{Choreo, ParserScribble, St4mp}
import choreo.syntax.St4mp.*
import choreo.view.{MermaidNPomset, SequenceChart}

object St4mpCaos extends Configurator[Scribble]:
  val name = "ST4MP: Session Types for Multilanguage Programming"
  override val languageName = "Fw Scribble"
  /** Parser for Scribble expressions. */
  val parser: String=>Scribble =
    ParserScribble.pp(ParserScribble.program,_).fold(x=>sys.error(x), x=>x)

  val examples = List(
    "a->b:X" -> "X from a to b;",
    "bad choice" -> "choice at a\n  { Abc from a to b; } or\n  { Cde from a to b; } or\n  { Efg from b to c; }",
    "choose and send" -> "choice at a {\n\tNumber from a to b;\n} or {\n  String from a to b;\n}\nDone from b to c;",
    "Ex.1" -> "Work from Master to Worker1;\nWork from Master to Worker2;\npar {\n  Done from Worker1 to Master;\n} and {\n  Done from Worker2 to Master;\n} ",
    "Ex.2" -> "rec Loop {\n  Work(cmd:String) from Master to Worker1;\n  Work(cmd:String) from Master to Worker2;\n  choice at Worker1 {\n    Work(cmd:String) from Worker1 to Worker2;\n    Done(res:Number) from Worker2 to Worker1;\n  } or {\n    None() from Worker1 to Worker2;\n  }\n  par {\n    Done(res:Number) from Worker1 to Master;\n  } and {\n    Done(res:Number) from Worker2 to Master;\n  }\n  continue Loop;\n}",
  )

  val widgets = List(
//    "Show" -> view(_.pp, Text),
    "Sequence diagram" -> view(sc=>SequenceChart(toChoreo(sc)), Mermaid),
    "Global" -> view(toGlobal(_).pp, Text),
    //    "Roles" -> view (roles(_).mkString("\n"), Text),
    "Projections" -> view((c:Scribble) =>
      St4mp.roles(c).toList.map(a=>s"$a -> ${proj(c)(using a).pp}").mkString("\n"), Text),
    //    "Projections 2" -> viewTabs((c:Scribble) =>
    //      St4mp.roles(c).toList.map(a=>a->proj(c)(using a).toString), Text),
    "Choreo" -> view (toChoreo(_).toString, Text),
    "Global automata" -> lts(c=>toChoreo(c), ChorDefSOS, _.toString, _.toString),
    "Local automata" -> viewMerms((sc:Scribble) =>
        val ch = toChoreo(sc)
        for a <- agents(ch).toList.sortWith(_.s<_.s) yield
          a.toString -> caos.sos.SOS.toMermaid(ChorDefSOS, ChorNoTauProj.proj(ch,a), _.toString, _.toString, 80) ),
//    "Global branching pomset" -> view( c =>
//      MermaidNPomset(Choreo2NPom(toChoreo(c))), Mermaid),
//    "Global branching pomset Txt" -> view( c =>
//      MermaidNPomset(Choreo2NPom(toChoreo(c))), Text),
    "Global pomsets" -> viewMerms( c =>
      Choreo2NPom(toChoreo(c)).refinements.zipWithIndex.map((p,n) => s"Pom ${n+1}"->MermaidNPomset(p))),
    "Local pomsets" -> viewMerms( c =>
      Choreo2NPom(toChoreo(c)).refinementsProj.zipWithIndex.map((ps,n) => s"Pom ${n+1}"->MermaidNPomset(ps))),
    "Scala APIs for pomsets"
      -> viewTabs(c =>
      val session = Session(Choreo2NPom(toChoreo(c)));
      session.modulesToCode:::List("All"->session.toString),
      Text),

  )



