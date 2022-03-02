package choreo.syntax

import choreo.syntax.Choreo.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by guillecledou on 10/02/2021
 */

/** Parser for [[Choreo]] expressions. */
object Parser extends RegexParsers:

  def parse(code: String): ParseResult[Choreo] =
    parseAll(program, code)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val upperCaseId: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val id: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val num: Parser[Int] = """[0-9][0-9]*""".r ^^ (n => n.toInt)

  def program: Parser[Choreo] =
    opt(choreography) ^^ {case c => c.getOrElse(End)}

  def par[A](parser: Parser[A]): Parser[A] = "(" ~> parser <~ ")"
  
  def agent: Parser[Agent] = lowerCaseId ^^ Agent.apply
  def agents: Parser[List[Agent]] = repsep(agent, ",")
  
  def message:Parser[Msg] = 
    ":" ~> rep1sep(id,",") ^^ {case ms => Msg(ms)}
  
  /**
   * A choreography expression
   * - Left associativity: ;,+,||
   * - Precedence: i,(c)>*>;>||>+
   *
   * @return
   */
  def choreography: Parser[Choreo] =
    maybeParallel ~ opt(choice) ^^ {
      case mb ~ Some(ch) => ch(mb)
      case mb ~ _ => mb
    }

  def choice: Parser[Choreo => Choreo] =
    "+" ~ maybeParallel ~ opt(choice) ^^ {
      case _ ~ mc ~ Some(more) => (lhs:Choreo) => more(Choice(lhs,mc))
      case _ ~ mc ~ _          => (lhs:Choreo) => Choice(lhs,mc)
    } |
    "[+]" ~ maybeParallel ~ opt(choice) ^^ {
      case _ ~ mc ~ Some(more) => (lhs:Choreo) => more(DChoice(lhs,mc))
      case _ ~ mc ~ _          => (lhs:Choreo) => DChoice(lhs,mc)
    }

  def maybeParallel: Parser[Choreo] =
    maybeSequence ~ opt(parallel) ^^ {
      case lhs ~ Some(pll) => pll(lhs)
      case lhs ~ None => lhs
    }

  def parallel: Parser[Choreo => Choreo] =
    "||" ~ maybeSequence ~ opt(parallel) ^^ {
      case _ ~ ms ~ Some(more) => (lhs: Choreo) => more(Par(lhs, ms))
      case _ ~ ms ~ _    => (lhs: Choreo) => Par(lhs, ms)
    }

  def maybeSequence: Parser[Choreo] =
    atomChoreography ~ opt(sequence) ^^ {
      case lhs ~ Some(seq) => seq(lhs)
      case lhs ~ _ => lhs
    }

  def sequence: Parser[Choreo => Choreo] =
    (";"|".") ~ atomChoreography ~ opt(sequence) ^^ {
      case _ ~ seq ~ Some(more) => (lhs: Choreo) => more(Seq(lhs, seq))
      case _ ~ seq ~ _          => (lhs: Choreo) => Seq(lhs, seq)
    }

  def atomChoreography: Parser[Choreo] =
    literal ~ opt("*") ^^ {
      case lit ~ l => if l.isDefined then Loop(lit) else lit
    }

  def literal: Parser[Choreo] =
    "("~>choreography<~")" |
    "0" ^^^ End |
    agent ~ ("\\?|!|(->)".r) ~ agent ~ opt(message) ^^ {
      case a ~ "?" ~ b ~ ms =>   In(  a, b, ms.getOrElse(Msg(List())))
      case a ~ "!" ~ b ~ ms =>   Out( a, b, ms.getOrElse(Msg(List())))
      case a ~ _   ~ b ~ ms =>   Send(List(a), List(b), ms.getOrElse(Msg(List())))
    }

