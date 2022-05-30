package choreo.syntax

import scala.annotation.tailrec

object St4mp:
  enum DataType:
    case Number
    case String
    case Boolean
    case Array(t:DataType)
    case Obj(clazz:String,attr:List[(String,DataType)])

  enum Scribble:
    case Comm(t:DataType, from:String, to:String)
    case Choice(at:String, choices:List[Scribble])
    case Par(gs: List[Scribble])
    case Seq(g1:Scribble, g2:Scribble)
    case Rec(x:String, g:Scribble)
    case Var(x:String)

  enum Global:
    case Comm(from:String, to:String, cont:List[(DataType,Global)])
    case Par(gs: List[Global])
    case Seq(g1:Global, g2:Global)
    case Rec(x:String, g:Global)
    case Var(x:String)
    case End

  enum Local:
    case Send(from:String, to:String, cont:List[(DataType,Local)])
    case Recv(from:String, to:String, cont:List[(DataType,Local)])
    case Par(ls:List[Local])
    case Seq(l1:Local, l2:Local)
    case Rec(x:String, l:Local)
    case Var(x:String)
    case End


  /////////////////////
  // Enriching types //
  /////////////////////

  extension (sc:Scribble)
    def pp: String = sc match
      case Scribble.Comm(t, from, to) => s"${t.pp} from $from to $to;"
      case Scribble.Choice(at, choices) => s"choice at $at ${choices.map(s=>s"{${s.pp}}").mkString(" or ")}"
      case Scribble.Par(gs) => s"par ${gs.map(s=>s"{${s.pp}}").mkString(" and ")}"
      case Scribble.Seq(g1, g2) => s"${g1.pp} . ${g2.pp}"
      case Scribble.Rec(x, g) => s"rec $x { ${g.pp} }"
      case Scribble.Var(x) => x

  extension (g:Global)
    def ~(g2:Global): Global = (g,g2) match
      case (Global.End,_) => g2
      case (_,Global.End) => g
      case _ => Global.Seq(g,g2)

    def pp: String = g match
      case Global.Comm(from, to, cont) =>
        val rest = cont.map((d,g2)=>s"${d.pp}.${g2.pp}")
        s"${from}_$to!?${if rest.size==1 then rest.head else rest.mkString("(",",",")")}"
      case Global.Par(gs) => gs.map(_.pp).mkString(" || ")
      case Global.Seq(g1, g2) => s"${g1.pp} . ${g2.pp}"
      case Global.Rec(x, g) => s"rec $x . (${g.pp})"
      case Global.Var(x) => x
      case Global.End => "end"

  extension (l:Local)
    def pp: String = l match
      case Local.Send(from, to, cont) =>
        val rest = cont.map((d,l2)=>s"${d.pp}.${l2.pp}")
        s"${from}_$to!${if rest.size==1 then rest.head else rest.mkString("(",",",")")}"
      case Local.Recv(from, to, cont) =>
        val rest = cont.map((d,l2)=>s"${d.pp}.${l2.pp}")
        s"${from}_$to?${if rest.size==1 then rest.head else rest.mkString("(",",",")")}"
      case Local.Par(ls) => ls.map(_.pp).mkString(" || ")
      case Local.Seq(l1, l2) => s"${l1.seqPP}.${l2.seqPP}"
      case Local.Rec(x, l) => s"rec $x . (${l.pp})"
      case Local.Var(x) => x
      case Local.End => "end"
    private def seqPP: String = l match
      case _:Local.Par | _:Local.Rec => s"(${l.pp})"
      case _ => l.pp

  extension (dt:DataType)
    def pp: String = dt match
      case DataType.Number => "Number"
      case DataType.String => "String"
      case DataType.Boolean => "Boolean"
      case DataType.Array(t) => s"[${t.pp}]"
      case DataType.Obj(clazz, attr) =>
        s"$clazz${if attr.isEmpty then ""
          else attr.map(x=>s"${x._1}: ${x._2.pp}").mkString("(",",",")")}"


  //////////////////////////////
  // Conversions and analysis //
  //////////////////////////////

  /** Converting a (well-formed) Scribble term to a Global term. */
  def toGlobal(scr: Scribble): Global = scr match
    case Scribble.Par(gs) => Global.Par(gs.map(toGlobal))
    case Scribble.Seq(g1, g2) => Global.Seq(toGlobal(g1), toGlobal(g2))
    case Scribble.Rec(x, g) => Global.Rec(x, toGlobal(g))
    case Scribble.Var(x) => Global.Var(x)
    case Scribble.Comm(t, from, to) => Global.Comm(from, to, List(t -> Global.End))
    case Scribble.Choice(at, choices) =>
      @tailrec
      def getAt(g: Global): (String, List[(DataType, Global)]) = g match
        case Global.Comm(`at`, to, list) => (to, list)
        case Global.Seq(Global.Comm(`at`, to, list), g2) => (to, list.map(x => x._1 -> x._2 ~ g2))
        case Global.Seq(Global.End, g2) => getAt(g2)
        case Global.Seq(Global.Seq(g1, g2), g3) => getAt(Global.Seq(g1, Global.Seq(g2, g3)))
        case _ => sys.error(s"In choices ${choices.map(x=>s"\"${x.pp}\"").mkString(", ")} the option \"${g.pp}\" does not start from $at.")

      val nxt = for c <- choices yield getAt(toGlobal(c)) // nxt = List( to -> List(cont1,cont2,...) -> Some(afterChoice)
      val to = nxt.head._1 // all choices should have the same destination, and at least one should exist
      Global.Comm(at, to, nxt.flatMap(_._2))

  /** Projecting a Scribble term to a Local term for a given role. */
  def proj(g: Scribble)(using r: String): Local = proj(toGlobal(g))

  /** Projecting a Global term to a Local term for a given role. */
  def proj(g: Global)(using r: String): Local = g match
    case Global.Comm(`r`, to, cont) => Local.Send(r, to, cont.map((x, y) => (x, proj(y))))
    case Global.Comm(from, `r`, cont) => Local.Recv(from, r, cont.map((x, y) => (x, proj(y))))
    case Global.Comm(_, _, cont) => proj(cont.head._2)
    case Global.Par(gs) => Local.Par(gs.map(proj))
    case Global.Seq(g1, g2) => Local.Seq(proj(g1), proj(g2))
    case Global.Rec(x, g) => Local.Rec(x, proj(g))
    case Global.Var(x) => Local.Var(x)
    case Global.End => Local.End

  /** Extracting all roles used in a Scribble term. */
  def roles(g: Scribble): Set[String] = g match
    case Scribble.Comm(_, from, to) => Set(from, to)
    case Scribble.Choice(at, choices) => choices.flatMap(roles).toSet + at
    case Scribble.Par(gs) => gs.toSet.flatMap(roles)
    case Scribble.Seq(g1, g2) => roles(g1) ++ roles(g2)
    case Scribble.Rec(_, g) => roles(g)
    case Scribble.Var(_) => Set()

  /** Converting a Scribble term to a Choreo term, to reuse previous results
   * (including API generation for Scala 3) */
  def toChoreo(sc: Scribble): Choreo =
    toChoreo(toGlobal(sc))

  /** Converting a Global choreography to Choreo. */
  def toChoreo(sc: Global): Choreo =
    toChoreoAux(sc)(using Nil)

  private def toChoreoAux(sc: Global)(using vars: List[(String, Global)]): Choreo = sc match
    case Global.Comm(from, to, cont) =>
      if cont.isEmpty then sys.error(s"Communication from $from to $to without any message")
//      if vars.nonEmpty then sys.error(s"Found communication from $from to $to, but expected var ${vars.head._1}")
      val x = for (dt, gl) <- cont yield
        simpleSeq(Choreo.Send(List(Agent(from)), List(Agent(to)), Msg(List(dt.pp))) , toChoreoAux(gl))
      x.tail.foldLeft[Choreo](x.head)(_ + _)
    case Global.Par(gs) =>
      if gs.isEmpty then sys.error(s"Parallel operation cannot be empty")
      val x = gs.map(toChoreoAux)
      x.tail.foldLeft[Choreo](x.head)(_ || _)
    case Global.Rec(x, g) =>
      Choreo.Loop(toChoreoAux(g)(using (x -> g) :: vars))
    case Global.Var(x) =>
      if !vars.headOption.exists(_._1 == x) then sys.error(s"Variable $x not expected. Queue: ${vars.map(_._1).mkString(",")}.")
      Choreo.End
    case Global.End =>
      if vars.nonEmpty then sys.error(s"Only infinite loops currently supported. Found `end`, but expected var ${vars.head._1}.")
      Choreo.End
    case Global.Seq(g1, g2) => simpleSeq(toChoreoAux(g1)(using Nil), toChoreoAux(g2))

  private def simpleSeq(c1:Choreo, c2:Choreo): Choreo = (c1,c2) match
    case (Choreo.End, _) => c2
    case (_, Choreo.End) => c1
    case _ => c1 > c2
