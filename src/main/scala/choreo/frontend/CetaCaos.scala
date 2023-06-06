package choreo.frontend

import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.frontend.widgets.WidgetInfo.VisualizeOpt
import caos.sos.SOS.*
import caos.sos.{BranchBisim, SOS}
import caos.view.*
import choreo.{Examples, sos}
import choreo.analysis.other.SyntAnalysis
import choreo.analysis.{DepGuarded, IEquiv, WellBranched, WellChannelled}
import choreo.api.{API, LocalAPI, Protocol, Session}
import choreo.npomsets.*
import choreo.pomsets.{Choreo2Pom, PomDefSOS, PomKeepSOS, Pomset}
import choreo.projection.*
import choreo.realisability.CC.*
import choreo.realisability.*
import choreo.sos.*
import choreo.sos.ChorSyncSOS.Interact
import choreo.sos.Network.*
import choreo.syntax.Choreo.{Action, agents}
import choreo.syntax.{Agent, Choreo, Msg}
import choreo.view.*
import choreo.view.ViewChoreo.*


object CetaCaos extends Configurator[Choreo]:

  val name = "Choreographic Extended Team Automata"
  override val languageName = "Choreography"
  /** Parser for Choreo expressions. */
  val parser: String=>Choreo = choreo.DSL.parse

  val examples = List(
    "Race (simple)"
      -> "// Race example\n(\n (ctr->r1,r2: start);\n (r1->ctr:finish ||\n  r2->ctr:finish)\n)*"
      -> "A controller starts 2 runners at the same time, and receives a finish message from each runner at a time.",
    "Gossip (bad)"
      -> "c->a:w; (a->b:g + c->b:w) +\nc->b:w; a->b:g"
      -> "Interaction protocol involving Alice, Bob and Carol:\n<ol>\n <li>Carol asks either Alice, Bob, or Alice then Bob to Work</il>\n <li> Alice Gossips to Bob if only one was asked.</il>\n</ol>\n(Taken from <a href=\"https://arxiv.org/abs/2210.08223\">https://arxiv.org/abs/2210.08223</a>, Ex. 2.3)",
    "Gossip (good)"
      -> "c->a:w;\n\t(a->b:g +\n   c->b:w; a->b:g) +\nc->b:w; a->b:g +\na->b:g"
      -> "Variation of the \"Gossip (bad)\".\n<ol>\n <li> Carol asks either Alice, Bob, Alice then Bob, [or none] to Work</li>\n <li> Alice Gossips to Bob [always]</li>\n</ol>\n(Taken from <a href=\"https://arxiv.org/abs/2210.08223\">https://arxiv.org/abs/2210.08223</a>)",
    "Toss" -> "(p->c:toss; c->p:head +\n p->c:toss ; c->p:tail)*",
    "Cast1" -> "(c->d; a->b; a->c +\n a->b; c->d; a->c)*",
    "Cast2" -> "((c->d; a->b +\n  a->b; c->d); a->c)*",
    "Cast3" -> "((c->d; a->b +\n  a->b; c->d))",
    "ab|cd" -> "a->b:m || c->d:m",
    "ab;cd" -> "a->b:m ; c->d:m",
    "Race (once, simple)"
      -> "// Race example\n(\n (ctr->r1,r2: start);\n (r1->ctr:finish ||\n  r2->ctr:finish)\n)",
    "Race (sync)" -> "// Race example\n(\n (ctr->r1,r2: start);\n (r1:run || r2:run); \n (r1->ctr:finish; r1:rest ||\n  r2->ctr:finish; r2:rest)\n)*;\nctr->r1,r2:goHome"
      -> "Experiment with synchronous messages",
    "Race (par-start)" -> "// Race example\n(\n (ctr->r1: start ||\n  ctr->r2: start);\n (r1:run||\n  r2:run); \n (r1->ctr: finish;r1:rest ||\n  r2->ctr: finish;r2:rest)\n)*"
      -> "Two runners in a race with a controller.",
    "Broken1"
      -> "(b->a:go;\n   (a->b:ok;a->b:ok2 +\n    a->b:no2 +\n    a->b:no);\n)*;\nc->b:leave;\nb->c:leave"
      -> "Small (previously) broken example when running the SOS semantics",
    "Broken2"
      -> "(\n (a->b:x + a->b:y) ;\n c->b:y\n )*"
      -> "simple broken example when running the SOS semantics",
    "Chat (bad attempt)" -> "(\nc->s:join;\ns->c:confirmJ;\n(c->s:msg;\n s->a:asl;\n   (a->s:grant;s->c:fwdmsg + a->s:reject))*;\nc->s:leave;\ns->c:leave\n)*"
      -> "Example from Coordination'20 and ICTAC'20 papers, between a client, a server, and an arbitrer.",
    "JoinLeave" -> "// join/leave x2\n((u1->s:join;u1->s:leave)* ||\n(u2->s:join;u2->s:leave)* )\n// + \n// (u1,u2->s:join; u1,u2->s:leave)*"
      -> "Simplified version of a join/leave server, highlighing issues/challenges.",
    "JoinLeave Sec" -> "// join/leave x2 - secure\n(u1->s:join;s->u1:confirm;u1->s:leave)* ||\n(u2->s:join;s->u2:confirm;u2->s:leave)* "
      -> "Simplified version of a join/leave server with confirmations, highlighing issues/challenges.",
    "JoinLeave Exp" -> "// join/leave x2\nforall n <- Nat,\n\t\t\t n > 0\n\t\t   sel subsetOf {1..n}:\n(\n  {u[i] | i<-sel} -> s: join;\n  ||{u[i] -> s: leave | i<-sel}\n)*\n// stype = join:[1..*]->[1]\n//         leave:[1]->[1]"
      -> "Experimental syntax capturing n-ary participants.",
    "Rc" -> "// R_c example\na->b:x;\n(b->c:x + b->d:x);\nc->d:x"
      -> "Rc example from the companion journal paper, to exemplify the encoding of choreographies into branching pomsets.",
    "Rd" -> "// R_d example\n((a->b:x; (b->a:x + b->d:x)) +\n (a->c:x; (c->a:x + c->d:x)));\nd->a:x"
      -> "Rd example from the companion journal paper, to exemplify the encoding of choreographies into branching pomsets.",
    "Rf" -> "// Rf example\n(a->b:yes||b->a:yes) +\n(a->b:no||b->a:no)"
      ->"Rf example from the companion journal paper. Either both Alice (a) and Bob (b) say 'yes' or they say 'no' to each other. Not realisable.",
    "Rg" -> "// Rg example\na->b:int;\n((b->a:yes + b->a:no)\n ||\n a->b:bool)"
      ->"Rg example from the companion journal paper. Alice (a) sends a number to Bob (b), and Bob replies both a 'yes/no' answer and a boolean. Realisable.",
    "Ri" -> "// Ri example\n(a->b:yes + a->b:no);\na->b:int"
      ->"Ri example from the companion journal paper. Alice (a) sends 'yes' or 'no' to Bob (b), and he replies with a number. Not well-formed but realisable.",
    "Ri (tree-like)" -> "// Ri example (tree-like)\n(a->b:yes;a->b:int) +\n(a->b:no; a->b:int)"
      ->"Variation of the Ri example from the companion journal paper, after moving the trailing actions inside the choice. Becomes both well-formed and realisable.",
    "Review (choreographic)"
      -> "// Review variation (choreographic)\n(c->a:r;\n (a->c:y + a->c:n);\n c->a:t\n ||\n c->b:r;\n (b->c:y + b->c:n);\n c->b:t\n) +\nc->a:t || c->b:t"
      ->"Variation of the requesting reviews example (with replication to be represented by a choreography): Carol (c) either sends Alice (a) and Bob (b) a review request (r), in which case both Alice and Bob communicate to Carol whether they recommend acceptance (y or n), or she does not (e.g., if the paper can be rejected without any review). In both cases, Carol will thank (t) Alice and Bob when their work is done.",
    "Review (strict)"
      -> "// Review example - stricter\n((c->a:r;\n (a->c:y+a->c:n) ||\n c->b:r;\n (b->c:y+b->c:n)\n) + 1)\n;\n(c->a:t || c->b:t)"
      -> "Simpler variation of the review process, where Carol (c) waits for both Alice (a) and Bob (b) to reply before sending a confirmation.",
    //    "loop" -> "(a->b:x+b->a:y)*",
    "Buyer-seller" -> ("// Buyer-seller protocol\nb1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n" +
      "(b2->s:ok;b2->s:string;s->b2:date + b2->s:quit)")
      -> "Two-buyers-protocol",
    "Streaming" -> "// Simple streaming protocol\n(d->r:bool||k->r:bool);\nr->c:bool;\n(d->r:bool||k->r:bool);\nr->c:bool"
      -> "Simple streaming protocol",
    "BS-ill-chan" -> ("// Buyer-seller (bad) variation\nb1->s:string;\n(s->b1:int;b1->b2:int || s->b2:int);\n" +
      "((b2->s:ok||b2->s:string);s->b2:date + b2->s:quit)")
      -> "Ill-channeled version of the buyer-seller protocol with parallel sends",
    "SS-ill-chan" -> "// Streaming (bad) variation\n((d->r:bool||k->r:bool);\n r->c:bool)\n||\n((d->r:bool||k->r:bool);\n r->c:bool)"
      -> "Ill-channeled version of the simple streaming protocol with parallel sends",
    "MW" -> "// Master-worker protocol\n(m->w1:d;w1->m:d) ||\n(m->w2:d;w2->m:d)"
      -> "Master-Workers protocol",
    "DV" -> "// Distributed voting protocol\n((a->b:y || a->c:y) +\n (a->b:n || a->c:n))   ||\n((b->a:y || b->c:y) +\n (b->a:n || b->c:n))   ||\n((c->a:y || c->b:y) +\n (c->a:n || c->b:n))"
      -> "Distribted Voting protocol with 3 participants",
    "C1" -> "// c1 example\n(a->b:x + a->c:x);\n(d->b:x + d->e:x)"
         -> "Example of a choreography included in the companion journal paper.",
    "C2" -> "// c2 example\n(a->b:x + c->b:x)* ||\n(c->a:x + c->b:x)"
         -> "Example of a choreography included in the companion journal paper",
//    "Ex.2.1 (not dep-guard)"-> "(a->b:x + a->c:x)*"->"Not dependently guarded example",
//    "Ex.2.2 (dep-guard)"-> "(a->b:x + b->a:x)*"->"Dependently guarded example",
    "ICE: Fig.5" -> "// Fig.5 (ICE)\na->b:x;\n(b->c:x+b->d:x);\nc->d:x"
      -> "Example in Fig.5 in the companion ICE 2022 paper.",
    "ICE: Fig.6" -> "// Fig.6 (ICE)\n((a->b:x ;(b->a:x + b->d:x))+\n(a->c:x ;(c->a:x + c->d:x))) ; d->a:x"
      -> "Example in Fig.6 in the companion ICE 2022 paper.",
    "ICE: Ex.4.1" -> "// Example 4.1 (ICE)\na->b:x;\n(b->a:x + b->a:y)"
      -> "Example 4.1 in the companion ICE 2022 paper.",
    "ICE: Ex.4.2" -> "// Example 4.1 (ICE)\n(a->b:x ; b->a:x)+\n(a->b:x ; b->a:y)"
      -> "Example 4.2 in the companion ICE 2022 paper.",
    "ICE: Ex.4.3" -> "// Example 4.1 (ICE)\na->b:x + a->b:x"
      -> "Example 4.3 in the companion ICE 2022 paper.",
    "ATM" -> "// ATM example\nc->a:auth;\na->b:authReq; (\n\tb->a:denied; a->c:authFailed\n  +\n  b->a:granted;(\n    c->a:quit\n    +\n   \tc->a:checkBalance;\n      (a->c:advert ||\n       (a->c:advert || b->a:getBalance); a->c:balance)\n    +\n    c->a:withdraw; a->b:authWithdrawal;\n      (b->a:allow; a->c:money + b->a:deny; a->c:bye)))"
      -> "ATM example from [Guanciale & Tuosto, Realisability of pomsets, JLAMP 2019]",
)
    :::
    Examples.examples2show.map(xy => toExample(xy._1 -> xy._2.toString))

  private def chor2pom(c:Choreo):Pomset = Choreo2Pom(c)
  private def chor2npom(c:Choreo):NPomset = Choreo2NPom(c)
  private def ceta2npom(c:Choreo):NPomset = CETA2NPom(c)

  import WellBranched.{show, toBool}

//  override val smallWidgets = List(
//    "Sequence Diagram (w/o extension)" -> view(xc => SequenceChart(xc._1), Mermaid),
//    "Extension" -> view(xc => xc._2.mkString(" / "), Text),
//  )

  type SType = Map[Msg,(Set[Int],Set[Int])]
  def joinMsg(m1:SType, m2:SType): SType =
    m1 ++ (for (m,(in,out))<-m2 yield m1.get(m) match
      case None => m->(in,out)
      case Some((in1,out1)) => m->(in++in1,out++out1))

  def messages(c:Choreo): SType = c match
    case Choreo.Send(as, bs, m) => Map(m -> (Set(as.size),Set(bs.size)))
    case Choreo.Seq(c1, c2) => joinMsg(messages(c1),messages(c2))
    case Choreo.Par(c1, c2) => joinMsg(messages(c1),messages(c2))
    case Choreo.Choice(c1, c2) => joinMsg(messages(c1),messages(c2))
    case Choreo.DChoice(c1, c2) => joinMsg(messages(c1),messages(c2))
    case Choreo.Loop(c1) => messages(c1)
    case Choreo.End => Map()
    //case Choreo.Internal(_,m) => Map(m -> (Set(1),Set(1)))
    case action: Action => Map()

  def showSType(c:Choreo): String =
    messages(c)
      .map(kv => s" - ${kv._1.pp.drop(1)}->[${
        kv._2._1.toList.sorted.mkString(",")
      }],[${
        kv._2._2.toList.sorted.mkString(",")
      }]")
      .mkString("\n")

  private var states:(Int,Map[Choreo,String]) = (1,Map())
  implicit def show(a:Choreo): String = states._2.get(a) match
    case Some(i) => i
    case None =>
      states = (states._1+1,states._2+(a -> states._1.toString))
      (states._1-1).toString

  val widgets = List(
    "reset" -> check(c => {states = (1,Map()); Nil}),

    "LTS: Global S-Choreo"
      -> lts((c:Choreo) => c, ChorSyncSOS, x => show(x), _.toString).expand,

    "LTS: Local Quotients - with 'rich' actions (NOT Component Automata)" ->
      viewMerms((ch: Choreo) =>
        IEquiv.checkRCRich(ch, ChorSyncSOS) match
          case Left(err) => sys.error(err)
          case Right((eqs, _)) =>
            implicit val eqs2 = eqs
            for a <- agents(ch).toList.sortWith(_.s < _.s) yield
              a.toString -> SOS.toMermaid(
                IEquiv.mkQuotientRich(a),
                IEquiv.get(ch, a), // initial state
                q => q.map(x => show(x)).mkString(","), // displaying states
                _.toString, // displaying labels
                80)), // max size
    "LTS: Composed quotients - with 'rich' actions" ->
      lts((c: Choreo) =>
        IEquiv.checkRCRich(c, ChorSyncSOS) match
          case Left(err) => sys.error(err)
          case Right(r) => NetSync.mkInit[Choreo](c,
            agents(c), ag => st => IEquiv.get(st, ag)(using r._1), Right(r._2.keySet))//messages(c))
            ,
          NetSync.sos(ag=>Some(_)), // the network semantics
        st => st.parts.zip(st.netSt.agents)
          .map((acts, ag) => s"$ag[" + acts.map(show(_)).mkString(",") + "]").mkString(","), // how to view states
        _.toString,
        80),

    "LTS: Local Quotients - with 'loose' actions (Component Automata)" ->
      viewMerms((ch: Choreo) =>
        IEquiv.checkRCLoose(ch, ChorSyncSOS) match
          case Left(err) => sys.error(err)
          case Right((eqs, _)) =>
            for a <- agents(ch).toList.sortWith(_.s < _.s) yield
              implicit val eqs2 = eqs
              a.toString -> SOS.toMermaid(
                IEquiv.mkQuotientLoose(a),
                IEquiv.get(ch, a), // initial state
                q => q.map(x => show(x)).mkString(","), // displaying states
                (m,isRcv) => m + (if isRcv then "?" else "!"), // displaying labels
                80)).expand, // max size
    "LTS: composed quotients - with 'loose' actions" ->
      lts((c: Choreo) =>
        IEquiv.checkRCLoose(c, ChorSyncSOS) match
          case Left(err) => sys.error(err)
          case Right(r) => NetSync.mkInit[Choreo](c,
            agents(c), ag => st => IEquiv.get(st, ag)(using r._1), Right(r._2.keySet))// messages(c))
            ,
          NetSync.sos(ag => act => Some(act.filter(ag)).filter(_.agents.nonEmpty)), // the network semantics
        st => st.parts.zip(st.netSt.agents)
          .map((acts, ag) => s"$ag[" + acts.map(show(_)).mkString(",") + "]").mkString(","), // how to view states
        _.toString,
        80),

    "LTS: composed quotients - with 'loose' actions, using instead synchr. type" ->
      lts((c: Choreo) =>
        IEquiv.checkRCLoose(c, ChorSyncSOS) match
          case Left(err) => sys.error(err)
          case Right(r) => NetSync.mkInit[Choreo](c,
            agents(c), ag => st => IEquiv.get(st, ag)(using r._1), Left(messages(c)))
            ,
          NetSync
        .sos(ag => act => Some(act.filter(ag)).filter(_.agents.nonEmpty)), // the network semantics
        st => st.parts.zip(st.netSt.agents)
          .map((acts, ag) => s"$ag[" + acts.map(show(_)).mkString(",") + "]").mkString(","), // how to view states
        _.toString,
        80),

    //    "Sync-type"
//      -> view(showSType, Text),
//
    "checking RC - with rich actions (extending equiv. w/o backtracking)"
      -> view((c: Choreo) =>
      IEquiv.checkRCRich(c, ChorSyncSOS).fold(x => x, IEquiv.show) +
        "\nSynch. Type:\n" + showSType(c)
      , Text),

    "checking RC - with loose actions (extending equiv. w/o backtracking)"
      -> view((c: Choreo) =>
      IEquiv.checkRCLoose(c, ChorSyncSOS).fold(x => x, IEquiv.show) +
        "\nSynch. Type:\n" + showSType(c)
      , Text),

    "I-equivalences (just indistinguishable + equiv. closure)"
      -> view((c:Choreo) =>
        IEquiv.show(IEquiv.buildEquiv(Set(c),ChorSyncSOS,Choreo.agents(c),Set()))(using show(_)) +
          "\nSynch. Type:\n" + showSType(c)
      , Text),

    "Realisability via bisimulation (rich actions)" // (NPomSOS/Causal + proj)"
      -> compareBranchBisim(
      ChorSyncSOS, // S-Choreo semantics
      NetSync.sos(ag=>act=>Some(act)), // local semantics
      x => x, // initial choreo
      c => // initial projection
        IEquiv.checkRCRich(c, ChorSyncSOS)(using show(_)) match
          case Left(err) => sys.error(err)
          case Right(r) => NetSync.mkInit[Choreo](c,
            agents(c), ag => st => IEquiv.get(st, ag)(using r._1), Right(r._2.keySet)), // messages(c)),
      show1 = show(_),
      show2 = ((st:caos.sos.Network.State[Set[Choreo],NetSync.NetState[Choreo]])=>
        st.parts.zip(st.netSt.agents).map((ss,a)=>s"$a:[${ss.map(show(_)).mkString(",")}]").mkString(",")),
      maxDepth = 500), // when to timeout

    "Realisability via bisimulation (loose actions)" // (NPomSOS/Causal + proj)"
      -> compareBranchBisim(
      ChorSyncSOS, // S-Choreo semantics
      NetSync.sos(ag => act => Some(act.filter(ag)).filter(_.agents.nonEmpty)), // Projected system's semantics (causal channels)
      x => x, // initial choreo
      c => // initial projection
        IEquiv.checkRCLoose(c, ChorSyncSOS)(using show(_)) match
          case Left(err) => sys.error(err)
          case Right(r) => NetSync.mkInit[Choreo](c,
            agents(c), ag => st => IEquiv.get(st, ag)(using r._1), Right(r._2.keySet)) //messages(c))
            ,
          show1 = show(_),
      show2 = ((st: caos.sos.Network.State[Set[Choreo], NetSync.NetState[Choreo]]) =>
        st.parts.zip(st.netSt.agents).map((ss, a) => s"$a:[${ss.map(show(_)).mkString(",")}]").mkString(",")),
      maxDepth = 500), // when to timeout

//    "checking RC - Variant (no equiv. extension)"
//      -> view((c: Choreo) =>
//          IEquiv.checkRCWithoutExt(c,ChorSyncSOS)(using get(_)) match
//            case Left(err) => err
//            case Right(r) => IEquiv.show(r)(using get(_))
//    , Text),
//    "LTS (full view)"
//      -> lts((c: Choreo) => c, ChorSyncSOS, _.toString, _.toString),
//    "CETA B-Pomset"
//      -> view[Choreo](c => MermaidNPomset(ceta2npom(c)), Mermaid),
//    "LTS: Local S-Choreo (Component Automata)" ->
//      viewMerms((ch: Choreo) =>
//        for a <- Choreo.agents(ch).toList.sortWith(_.s < _.s) yield
//          a.toString -> SOS.toMermaid(ChorSyncSOS, ChorSyncProj.proj(ch, a), _ => " ", _.toString, 80)),
//    "Realisability via bisimulation" // (NPomSOS/Causal + proj)"
//      -> compareBranchBisim(
//      ChorSyncSOS, // S-Choreo semantics
//      Network.sosSync(ChorSyncSOS), // Projected system's semantics (causal channels)
//      x => x, // initial choreo
//      ch => mkNetSync[Choreo](ch, ChorSyncProj, messages(ch)), // initial projection
//      maxDepth = 500), // when to timeout

//    "Testing"
//      -> view(c => ),


    //    "Test 1: Local async"
//      -> lts(ch => Network.mkNetCS[Choreo](ChorDefProj.allProj(ch)),
//      Network.sosCS(ChorDefSOS),
//      _ => " ", _.toString),
//    "Test 2: Local async step-wise"
//      -> steps(ch => Network.mkNetCS[Choreo](ChorDefProj.allProj(ch)),
//      Network.sosCS(ChorDefSOS),
//      // _ => " ",
//      _.toString, Text),
//    "Test 3: Local async NET"
//      -> lts(ch => NetCausal.mkInit(ChorDefProj.allProj(ch).toList),
//      NetCausal.sos(ChorDefSOS),
//       _ => " ",
//      _.toString),
//    "Test 4: Local async NET step-wise"
//      -> steps(ch => NetCausal.mkInit(ChorDefProj.allProj(ch).toList),
//      NetCausal.sos(ChorDefSOS),
//      // _ => " ",
//      _.toString, Text),


    "Sequence Diagram" -> view[Choreo](xc => SequenceChart(xc), Mermaid).moveTo(1),

//    "Extension" -> view[XChoreo](xc => xc._2.mkString(" / "), Text).moveTo(1),
//    "CETA B-Pomset txt"
//      -> view[Choreo](c => ceta2npom(c).toString, Text).expand,

//    "Async Global B-Pomset (not in use)"
//      -> view[Choreo](c => MermaidNPomset(chor2npom(c)), Mermaid),
//
//    "Semantics (via B-Pomsets)"
//      -> steps(xc => ceta2npom(xc), NPomDefSOS, MermaidNPomset.apply, Mermaid),
//
//    "Semantics: Global Sync (via Choreo)"
//      -> steps(xc => xc, ChorSyncSOS, _.toString, Text),
//
//    "Semantics: Local Sync (via Choreo)"
//      -> steps(ch => mkNetSync[Choreo](ch,ChorSyncProj,messages(ch)),
//               Network.sosSync(ChorSyncSOS),
//               _.toString,
//               Text),
//
////    "Realisability via bisimulation" // (NPomSOS/Causal + proj)"
////      -> compareBranchBisim(
////      ChorSyncSOS, // S-Choreo semantics
////      Network.sosSync(ChorSyncSOS), // Projected system's semantics (causal channels)
////      x=>x, // initial choreo
////      (xc: Choreo) => mkNetCS(xc, ChorSyncProj), // initial projection
////      maxDepth = 1000), // when to timeout
//
//  //    "Global B-Pomset (mermaid-txt)"
////      -> view(xc=>MermaidNPomset(chor2npom(xc)), Text),
//    "Local B-Pomset (Component Automata as BP)"
//      //      -> view( c => MermaidNPomset(chor2npom(c).projectAll), Mermaid),
//      -> viewMerms((xc: Choreo) => ceta2npom(xc).projectMap.toList.map((a, b) => (a.toString, MermaidNPomset(b)))),
//    "Local S-Choreo"
//      //      -> view( c => MermaidNPomset(chor2npom(c).projectAll), Mermaid),
//      -> viewTabs((xc: Choreo) => ChorSyncProj.allAProj(xc).toList.map((xy:(Agent,Choreo))=> xy._1.toString->xy._2.toString), Text),
//    //    "Global LTS (from choreo)"
////      -> lts(xc=>xc._1, ChorDefSOS, _=>" ", _.toString),
//    "LTS: Global BP"
//      -> lts(xc => ceta2npom(xc), NPomDefSOS, _ => " ", _.toString),
//    "LTS: Local Composed S-Choreo"
//      -> lts(ch => mkNetSync[Choreo](ch, ChorSyncProj, messages(ch)),
//             Network.sosSync(ChorSyncSOS),
//             _ => " ", _.toString),
//    "LTS info: Global BP"
//      -> view((xc:Choreo) => {
//          val bp = ceta2npom(xc).minimized
//          val bpstat = s"BP events: ${bp.events.toSet.size}\nBP dependencies: ${bp.pred.map(kv=>kv._2.size).sum}"
//          val (st,eds,done) = SOS.traverse(NPomDefSOS,bp,2000)
//          if !done then s"Stopped after traversing 2000 states\n$bpstat"
//          else s"States: ${st.size}\nEdges: $eds\n$bpstat"
//        },
//        Text),

//    "LTS: Local S-Choreo 2" ->
//      viewTabs((ch: Choreo) =>
//        for a <- Choreo.agents(ch).toList.sortWith(_.s < _.s) yield
//          a.toString -> caos.sos.SOS.toMermaid(ChorSyncSOS, ChorSyncProj.proj(ch, a), _ => " ", _.toString, 80),Text),


//    "Local LTS (from choreo)" -> viewMerms((xc: XChoreo) =>
//      val ch = xc._1
//      for a <- Choreo.agents(ch).toList.sortWith(_.s < _.s) yield
//        a.toString -> caos.sos.SOS.toMermaid(ChorDefSOS, ChorNoTauProj.proj(ch, a), _ => " ", _.toString, 80)),

//    "LTS: Local from B-Pomset (Component Automata)" -> viewMerms((xc: Choreo) =>
//        for a <- Choreo.agents(xc).toList.sortWith(_.s < _.s) yield
//          a.toString -> caos.sos.SOS.toMermaid(
//            NPomDefSOS,
//            NPomDefProj.proj(ceta2npom(xc),a),
//            _ => " ",
//            _.toString, 80)
//        ),

  //    "Global pomsets" -> viewMerms(c =>
//      Choreo2NPom(c).refinements.zipWithIndex.map((p, n) => s"Pom ${n + 1}" -> MermaidNPomset(p))),
//    "Local pomsets" -> viewMerms(c =>
//      Choreo2NPom(c).refinementsProj.zipWithIndex.map((ps, n) => s"Pom ${n + 1}" -> MermaidNPomset(ps))),

//    "Dependently Guarded (Choreo only)"
//      -> view(xc => DepGuarded(xc) match
//          case Left(value) => s"Not dependently guarded: ${value.mkString(", ")}"
//          case Right(_) => "OK"
//        , Text),

//    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
//      -> compareBranchBisim(ChorDefSOS, Network.sosMS(ChorDefSOS), x => x, mkNetMS(_, ChorNoTauProj)),
//    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
//      -> compareBranchBisim(ChorDefSOS, Network.sosCS(ChorDefSOS), x => x, mkNetCS(_, ChorNoTauProj)),


//    "Realisability via bisimulation (choreo: no-tau-proj + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorNoTauProj)),
//    //    "Realisability via branch-bisimulation (default proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorDefProj)),
//    //    "Realisability via branch-bisimulation (many-taus proj+SOS w/o taus)"
//    //      -> compareBranchBisim(ChorDefSOS,Network.sos(postponeTaus(ChorDefSOS)),id,Network(_,ChorManyTausProj)),
//    "Realisability via bisimulation (choreo: no-tau-proj + CAUSAL net + default SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorNoTauProj)),
//
//    "Realisability via trace equivalence (MSet) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosMS(ChorDefSOS),x=>x,mkNetMS(_,ChorDefProj)),
//    "Realisability via trace equivalence (Causal) (choreo: default proj+SOS)"
//      -> compareTraceEq(ChorDefSOS,Network.sosCS(ChorDefSOS),x=>x,mkNetCS(_,ChorDefProj)),
//    //"Realisability via branch-bisimulation (NPomSOS + proj with interclosure all-in-1)"
//    //  -> compareBranchBisim(NPomDefSOS,NPomDefSOS,chor2npom,chor2npom(_).icnpom.head.getPom),
//    "Realisability via branch-bisimulation (NPomSOS + proj)"
//      -> compareBranchBisim(NPomDefSOS,Network.sosMS(NPomDefSOS),chor2npom,(c:Choreo) => mkNetMS(chor2npom(c),NPomDefProj)),
//    //"Petri Net"
//    //  -> Visualize((c:Choreo)=>View(MermaidPN(choreo.petrinet.OneSafeColouredPN.pn1)),Mermaid,id), //hardcoded pn
//    //
//
//    "CC2-NPOM NPomset Inter-Closure"
//      -> //viewMerms((c:Choreo) => for (p,id)<-chor2npom(c).icpom.zipWithIndex yield s"IC${id}" -> p),
//         VisualizeOpt(showIC, Mermaid, chor2npom(_).icnpom),
//    "CC2-NPOM-plus-plus Merge Interclosure"
//      -> view( c=> MermaidNPomset(chor2npom(c).mergeIC), Mermaid),
//        //Visualize(viewNPomMerm,Mermaid, chor2npom(_).mergeIC),
//    "CC2-NPOM NPomset (Simplified)"
//      -> view( c=> MermaidNPomset(chor2npom(c).simplifyChoices), Mermaid),
//        //Visualize(viewNPomMerm, Mermaid, chor2npom(_).simplifyChoices),
//    "CC2-NPOM NPomset Inter-Closure (Simplified)"
//      -> VisualizeOpt(showIC, Mermaid, (c:Choreo) => ICNPOM(chor2npom(c))(using true)),
//    "CC2-NPOM-plus-plus Merge Interclosure (Simplified)"
//      -> view( (c:Choreo) => MermaidNPomset(Merge.compose(ICNPOM(chor2npom(c))(using true).head)), Mermaid),
//         //Visualize(viewNPomMerm, Mermaid, (c:Choreo) => Merge.compose(ICNPOM(chor2npom(c))(using true).head)),
//    //"CC2-NPOM Summary (Simplified)"
//    //  -> Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2npom),
//    //"Inter-Closure (Emilio)"
//    //  -> Visualize(viewEICPomsMerm, chor2npom(_).einterclosure),
//    "CC2-POM Global Refinements"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements),
//    "CC2-POM Projections per Refinement"
//      -> VisualizeOpt(showRefProj,Mermaid,chor2npom(_).refinementsProj),
//    "CC2-POM Inter-Closures with Result"
//      -> VisualizeOpt(showICWithResMermaid,Mermaid,chor2npom(_).cc2),
//    "CC2-POM Summary"
//      -> view(c=> CC.ppcc2(chor2npom(c).cc2), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc2(r)),Text,chor2npom(_).cc2),
//    "CC3-POM Global Prefixes"
//      -> VisualizeOpt(showRef,Mermaid,chor2npom(_).refinements.map(r=>NPomDAG.prefixes(r).toList).flatten.distinct),
//    "CC3-POM Summary"
//      -> view(c=> CC.ppcc3(chor2npom(c).cc3), Text),
//         //Visualize((r:CCPomInfo)=>View(CC.ppcc3(r)),Text,chor2npom(_).cc3),
//    //"Realisability NPomset (experiments)"
//    //  -> Visualize((b:Boolean) => Text(b.toString), chor2npom(_).realisable),
////    "Project NPomset at a"
////      -> Visualize(viewNPomMerm, chor2npom(_).project(Agent("a"))),
////    "Pomset as Text"
////      -> Visualize(viewPomTxt,chor2pom),
////    "Simulate Choreo (basic)"
////      -> Simulate(ChorBasicSOS,viewChorTxt,id),


         //Simulate(ChorDefSOS,viewChorTxt,Text,id),
//    "Simulate Network of Choreo"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorDefProj,x=>x),
//    "Simulate Network of Choreo (no-taus)"
//      -> simulateNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
//    "Simulate Causal Network of Choreo (no-taus)"
//      -> simulateCNet(ChorDefSOS,viewChorTxt,ChorNoTauProj,x=>x),
//    "Visualize a Causal Network of Choreo (no-taus)"
//      -> view((ch:Choreo) => viewCSNetConc[Choreo](Network.mkNetCS(ChorNoTauProj.allProj(ch)), viewChorTxt).code, Text),
//         //Visualize(x=>viewCSNetConc[Choreo](x,viewChorTxt),Text, (ch:Choreo) => Network.mkNetCS(ChorNoTauProj.allProj(ch))),
////    "Simulate Network of Choreo (many-taus)"
////      -> simulateNet(ChorManyTausSOS,viewChorTxt,ChorManyTausProj,id),
////    "Simulate Network of Choreo (default w/o taus)"
////      -> simulateNet(postponeTaus(ChorDefSOS),viewChorTxt,ChorDefProj,id),
////    "Simulate Network of Choreo (many-taus w/o taus)"
////      -> simulateNet(postponeTaus(ChorManyTausSOS),viewChorTxt,ChorManyTausProj,id),


//    "Simulate B-Pomset (Txt)"
//      -> steps(chor2npom, NPomDefSOS, _.toString, Text),
        //Simulate(NPomDefSOS,viewNPomMerm,Mermaid,chor2npom),
//    "Simulate Pomset (keeper)"
//      -> Simulate(PomKeepSOS,viewPomMerm,chor2pom),
    //"Simulate NPomset Interclosure"
    // -> Simulate(NPomDefSOS,viewNPomMerm,chor2npom(_).icnpom.head.getPom) ,
//    "Simulate B-Pomset Network"
//      -> simulateNet(NPomDefSOS,(p:NPomset)=>View(p.toString),NPomDefProj,chor2npom) ,


//    "Choreo vs B-Pomset (find bisimulation - no loops with infinte states)"
//      -> compareBranchBisim(ChorDefSOS,NPomDefSOS,xc=>xc._1,xc=>chor2npom(xc._1),200),
//
//    "ALL: Well-Branched" ->
//      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
//        name + ": " + WellFormedness.wellBranched(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
//        Text),
//    "ALL: Well-Channelled" ->
//      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
//        name + ": " + WellFormedness.wellChanneled(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
//        Text),
//    "ALL: Tree-like" ->
//      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
//        name + ": " + WellFormedness.treeLike(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
//        Text),
//    "ALL: Choreographic" ->
//      viewAll((cs: Seq[(String, XChoreo)]) => (for (name, xc) <- cs yield
//        name + ": " + WellFormedness.choreographic(chor2npom(xc)).getOrElse("OK")).mkString("\n"),
//        Text),
//    "ALL: Well-formed"
//      -> viewAll((cs: Seq[(String, XChoreo)]) => (for (name, c) <- cs yield
//      val wb = WellFormedness.wellBranched(chor2npom(c))
//      val wc = WellFormedness.wellChanneled(chor2npom(c))
//      val tl = WellFormedness.treeLike(chor2npom(c))
//      val ch = WellFormedness.choreographic(chor2npom(c))
//      if wc.isEmpty && wb.isEmpty && tl.isEmpty && ch.isEmpty then s"$name: ok"
//      else s"$name: ${
//        if wb.isEmpty then "" else s"[${wb.get}] NOT "
//      }well branched , ${
//        if wc.isEmpty then "" else s"[${wc.get}] NOT "
//      }well channeled, ${
//        if tl.isEmpty then "" else s"[${tl.get}] NOT "
//      }tree-like, ${
//        if ch.isEmpty then "" else s"[${ch.get}] NOT "
//      }choreographic."
//      ).mkString("\n"),
//      Text
//    ),



      /// to drop for ICE
//    "Choreo (old): Syntactic realisability"
//      -> view(xc =>
//            val c = xc._1
//            val wb = WellBranched(c)
//            val wc = WellChannelled(c)
//            if wc.toBool && wb.toBool then "OK" else
//              s"${if !wb.toBool then s"Not well branched:\n  - ${wb.show.drop(7)}\n" else ""}${
//                  if !wc.toBool then s"Not well channeled:\n  - ${wc.show.drop(7)}\n" else ""}"
//          , Text),
//    "Choreo (old): ALL - Syntactic realisability"
//      -> viewAll( (cs:Seq[(String,XChoreo)]) => (for (name,xc) <- cs yield
//              val c = xc._1
//              val wb = WellBranched(c).toBool
//              val wc = WellChannelled(c).toBool
//              val dg = DepGuarded(c).isRight
//              if wc && wb && dg then s"$name: ok"
//              else s"$name: ${if wb then "" else "NOT "}well branched, ${if wc then "" else "NOT "}well channeled, ${if dg then "" else "NOT "}dependently guarded."
//            ).mkString("\n"),
//            Text
//          ),

//    "Choreo (def) vs Pomset (def)"
//      -> compareBranchBisim(ChorDefSOS,PomDefSOS,id,chor2pom),
//    "Realisability via branch-bisimulation (default proj+SOS)"
//      -> compareBranchBisim(ChorDefSOS,Network.sos(ChorDefSOS),id,Network(_,ChorDefProj)),

//    "Experiments with syntactic realisability"
//      -> Visualize(Text,SyntAnalysis.realisablePP)
//    "Default realisability of all examples"
//      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//          yield s"- $s: "+choreo.DSL.realisable(c)).mkString("\n")),
//    "Choreo vs. Pomsets of all examples"
//      -> Visualize(Text, (_:Choreo)=>(for (s,c)<-examples
//        yield s"- $s: "+BranchBisim.findBisim(c,chor2pom(c))(using ChorDefSOS,PomDefSOS,50).isRight).mkString("\n")),
//    "Visualize projections of Pomsets"
//      -> Visualize(viewSeqMerm[Pomset](_,viewPomMerm), (c:Choreo) => PomDefProj.allProj(chor2pom(c)))
    //...
  )

  def simulateNet[S](sos:SOS[Action,S],
                     sview:S=>View,
                     proj:Projection[_,S],
                     enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkMS[S]] =
    steps((c:Choreo)=> Network.mkNetMS(enc(c),proj),
          Network.sosMS[S](sos),
          x => ViewChoreo.viewNetConc(x,sview).code,
          Text)
      //Simulate(Network.sosMS(sos),net=>ViewChoreo.viewNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetMS(enc(c),proj))
//
  def simulateCNet[S](sos:SOS[Action,S],
                      sview:S=>View,
                      proj:Projection[_,S],
                      enc:(Choreo=>S)): WidgetInfo[Choreo] = //Simulate[Choreo,Action,NetworkCausal[S]] =
    steps((c:Choreo)=> Network.mkNetCS(enc(c),proj),
      Network.sosCS(sos),
      x => ViewChoreo.viewCSNetConc(x,sview).code,
      Text)
    //Simulate(Network.sosCS(sos),net=>ViewChoreo.viewCSNetConc(net,sview), Text, (c:Choreo)=>Network.mkNetCS(enc(c),proj))

//  def visualizeMNet[S](sview:S=>Mermaid,
//                       proj:Projection[_,S],
//                       enc:(Choreo=>S)): Simulate[Choreo,Action,Network[S]] =
//    Visualize(net=>ViewChoreo.viewNetConc(net,sview), (c:Choreo)=>Network(enc(c),proj))

