package choreo.view

import choreo.petrinet.OneSafeColouredPN
import choreo.petrinet.OneSafeColouredPN.*
import choreo.syntax.Choreo.{In, Out}


/**
 * One safe coloured petri net to Mermaid
 */
@deprecated
object MermaidPN :

  def apply(pn:OneSafeColouredPN):String =
    s"""
       |graph LR
       |
       |%% places
       |${mkPlaces(pn.places)}
       |
       |%% trans
       |${mkTransitions(pn.transitions)}
       |
       |%% arcs
       |${mkArcs(pn.transitions)}
       |
       |%% styles
       |classDef marked   fill:#9ea,stroke:#333,stroke-width:2px;
       |classDef unmarked fill:#fff,stroke:#333,stroke-width:2px;
       |
       |class ${pn.marking.map(p=> s"""p$p""").mkString(",")} marked
       |class ${(pn.places.map(_.id) -- pn.marking).map(p=>s"""p$p""").mkString(",")} unmarked
       |""".stripMargin


  def mkPlaces(places:Set[Place]):String =
    places.map(mkPlace).mkString("\n")

  def mkPlace(p:Place):String =
    s"""p${p.id}((p${p.id}))"""

  def mkTransitions(trans:Set[Trans]):String =
    trans.map(mkTransition).mkString("\n")

  def mkTransition(t:Trans):String =
    s"""t${t.id}[" ${mkTransID(t.id)} <br> ${mkChannel(t.channel)} <br> ${mkPre(t.pre)} "]"""

  def mkChannel(c:In | Out):String =
    c.toString

  def mkPre(pre:DNF[PlaceId]):String =
    pre.options.map(mkOption).mkString(" or ")

  def mkOption(op:Set[BVar[PlaceId]]):String = op.toList match
    case Nil => ""
    case v::Nil => mkBVar(v)
    case l => l.map(mkBVar).mkString("("," & ",")")

  def mkBVar(v:BVar[PlaceId]):String = v match
    case NVar(name) => s"""¬p${name}"""
    case Var(name) => s"""p${name}"""

  def mkTransID(n:TransId):String =
    s"""t$n"""

  def mkArcs(trans:Set[Trans]):String =
    trans.map(mkArcsOfTransition).mkString("\n")

  def mkArcsOfTransition(t:Trans):String =
    t.prePlaces.map(p=>mkInArcOfTransition(p,t)).mkString("\n") ++ "\n" ++
      t.post.map(p=>mkOutArcOfTransition(p,t)).mkString("\n")

  def mkInArcOfTransition(p:PlaceId, t:Trans):String =
    s"""p$p --> t${t.id}"""

  def mkOutArcOfTransition(p: (PlaceId, PlaceId | Boolean), t: Trans):String =
    val (postPlace, fun) = p
    fun match
      case false => s""" p$postPlace <-.- t${t.id}""" //s"""t${t.id} -.-> p$postPlace"""
      case true => s"""t${t.id} -- $fun --> p$postPlace"""
      case _:Int => s"""t${t.id} -- p$fun --> p$postPlace"""