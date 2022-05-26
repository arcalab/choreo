package choreo.syntax

import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Numbers.*
import cats.syntax.all.*
import P.*
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import choreo.syntax.St4mp.DataType
import choreo.syntax.St4mp.*
import choreo.syntax.St4mp.Scribble.Comm

import scala.language.postfixOps

object ParserScribble:

  def program: P[Scribble] = scribble.surroundedBy(sps)

  /** Parses a choreo expression */
  def scribble: P[Scribble] = P.recursive(recScr =>
    def choice: P[Scribble] =
      (string("choice")~string("at").surroundedBy(sps)*>
        role~sps~((char('{')~sps)*>recScr<*(sps~char('}'))).repSep(string("or").surroundedBy(sps).backtrack))
        .map(x => Scribble.Choice(x._1._1,x._2.toList))

    def par: P[Scribble] =
      (string("par")~sps *>
        ((char('{')~sps)*>recScr<*(sps~char('}'))).repSep(string("and").surroundedBy(sps).backtrack))
        .map(x => Scribble.Par(x.toList))

    //    def seq: P[Global] =
//      (lit ~ char('.').surroundedBy(sps) ~ recScr)
//        .map(x => Global.Seq(x._1._1,x._2))

    def rec: P[Scribble] =
      (string("rec") *> typeName.surroundedBy(sps) ~
        char('{') ~ recScr.surroundedBy(sps) ~ char('}'))
        .map(x => Scribble.Rec(x._1._1._1, x._1._2))

    def lit: P[Scribble] =
      choice | par | rec | cont | comm

    (lit~sps~lit.repSep0(sps))
      .map( x => x._2.foldLeft[Scribble](x._1._1)(Scribble.Seq(_,_)) )
//    (lit ~ ((char('.').surroundedBy(sps).backtrack *> recScr)?))
//        .map(x => x._2 match
//          case None => x._1
//          case Some(rest) => GlobalScr.Seq(x._1,rest)
//        )
  )

//  def end: P[GlobalScr] =
//    string("end").as(GlobalScr.End)

  def comm: P[Scribble] =
    (data ~ string("from").surroundedBy(sps) ~ role ~ string("to").surroundedBy(sps) ~ role <* (sps ~ char(';')))
      .map(x => Comm(x._1._1._1._1,x._1._1._2,x._2))

  def cont: P[Scribble] =
    (string("continue") *> typeName.surroundedBy(sps) <* char(';'))
      .map(Scribble.Var.apply)




  // simplification for now
  def dataOld: P[DataType] =
    name.map(s => DataType.Obj(s,Nil))

  def data: P[DataType] = P.recursive( dtRec =>
    def args: P[List[(String,DataType)]] =
      (char('(') *> (name ~ char(':').surroundedBy(sps) ~ dtRec).repSep0(string(",")) <* char(')'))
        .map(_.map(y => y._1._1 -> y._2))

    string("Number").as(DataType.Number).backtrack |
    string("String").as(DataType.String).backtrack |
    string("Boolean").as(DataType.Boolean).backtrack |
    (char('[') *> dtRec.surroundedBy(sps) <* char(']')).map(DataType.Array.apply) |
    (name~(args?).surroundedBy(sps))
      .map(x => DataType.Obj(x._1, x._2.getOrElse(Nil)))
  )

  def role:P[String] = name



  /// auxiliary parsers
  /** Parser for a sequence of spaces or comments */
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace | comment).rep0.void

  extension (p:P[_]) def ws = p.surroundedBy(sps)

  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  def typeName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  def name: P[String] =
    ((P.charIn('A' to 'Z') | P.charIn('a' to 'z')) ~ alphaDigit.rep0).string

  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A],str:String): Either[String,A] =
    parser.parseAll(str) match //.fold(e=>prettyError(str,e), x=>x)
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  def prettyError(str:String,err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${"-".repeat(y+1)+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"
