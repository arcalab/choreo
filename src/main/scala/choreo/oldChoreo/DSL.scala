package choreo.oldChoreo

import choreo.oldChoreo.backend.Dot._
import choreo.oldChoreo.backend.DotPomsets._
import choreo.oldChoreo.backend.Mermaid.MermaidOps
import choreo.oldChoreo.backend.MermaidChoreography._
import choreo.oldChoreo.backend.Show
import choreo.oldChoreo.common.{DefinitionException, ParsingException}
import choreo.oldChoreo.semantics.{PomsetFamily, Semantics}
import choreo.oldChoreo.syntax.GlobalContext.{Context, Ctx}
import choreo.oldChoreo.syntax.{Interpreter, Parser, Program}

/**
 * Created by guillecledou on 29/10/2020
 */


object DSL:

  def parse(program: String): Program = Parser.parse(program) match
    case Parser.Success(res, _) => res
    case f: Parser.NoSuccess => throw new ParsingException("Parser failed: " + f)

  lazy val p1 =
    """
      |def fifo<m>(i)(o) = {
      | get(i),und(m) -> m:=i
      | get(m) -> o:=m
      |}
      |
      |def join(a,b)(c) = {
      | get(a,b) -> c:={a,b}
      |}
      |
      |def dupl<x,y>(a)(b,c) = {
      | get(a) -> x:=a, y:=a
      | get(x) -> b:=x
      | get(y) -> c:=y
      |}
      |
      |a>fifo(m)>b ; b>dupl(m1,m2)>c,d + a>fifo(m3)>c
      |""".stripMargin

    lazy val p2 =
      """
        |def send<w>(dp,kp)(k) = {
        | get(dp)-> w:=dp
        | get(w,kp) -> k:={w,kp}
        |}
        |
        |def notify<x,y,z>(k)(c1,c2) = {
        | get(k)-> x:=k
        | get(x)->y:=x
        | get(y)->z:=y
        | get(z)->c1:=z,c2:=z
        |}
        |
        |def check<z>(m)(m)={
        | get(z)-> m:=z, z:=z
        |}
        |
        |(dataProd,keyProd >send(w)> kernel ; kernel > notify(x,y,z)>cons1,cons2)* || (monitor>check(z)>monitor)*
        |""".stripMargin


    def test(code: String) = Interpreter(parse(code)) match
      case Left(err) => println(s"[${err.pos}] $err")
      case Right((choreo, channels)) => {
        println((semantics(choreo, channels).toDot))
        println(choreo.toMermaid)
      }

    def parseAndValidate(code: String): (Choreography, Ctx[Channel]) = Interpreter(parse(code)) match
      case Left(err) => throw new DefinitionException(Show(err))
      case Right((choreo, channels)) => (choreo, channels)

    def semantics(choreography: Choreography, channels: Ctx[Channel]): PomsetFamily =
      Semantics(choreography)(channels)

    def dot(pf: PomsetFamily): String = pf.toDot
