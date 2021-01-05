package choreo.syntax

import choreo.syntax.Program._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

/**
 * Created by guillecledou on 28/10/2020
 */


object Parser extends RegexParsers:

  def parse(code: String): ParseResult[Program] =
    parseAll(program, code)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r

  val upperCaseId: Parser[String] = """[A-Z][a-zA-Z0-9_]*""".r
  val lowerCaseId: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val id: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val num: Parser[Int] = """[0-9][0-9]*""".r ^^ (n => n.toInt)

  def program: Parser[Program] =
    rep1(pos(channelDef)) ~ pos(choreography) ^^ {
      case cs ~ c => Program(cs, c)
    }

  def par[A](parser: Parser[A]): Parser[A] = "(" ~> parser <~ ")"

  def pos[A <: Positional](parser: Parser[A]): Parser[A] = positioned(parser)

  def variable: Parser[Variable] = lowerCaseId ^^ Variable

  def variables: Parser[List[Variable]] = repsep(pos(variable), ",")

  def oneOrMoreVars: Parser[List[Variable]] = rep1sep(pos(variable), ",")

  def channelDef: Parser[ChannelDef] =
    "def" ~ lowerCaseId ~ opt("<" ~> oneOrMoreVars <~ ">") ~ par(variables) ~ par(variables) ~ "=" ~
      "{" ~ rep1(pos(guardedCommand)) ~ "}" ^^ {
      case _ ~ name ~ ms ~ ins ~ outs ~ _ ~ _ ~ gcs ~ _ => ChannelDef(name, ms.getOrElse(List()), ins, outs, gcs)
    }

  def guardedCommand: Parser[GuardedCommandDef] =
    guards ~ "->" ~ commands ^^ { case g ~ _ ~ c => GuardedCommandDef(g, c) }

  def guards: Parser[List[GuardDef]] =
    repsep(pos(guard), ",")

  def guard: Parser[GuardDef] =
    "get" ~> par(oneOrMoreVars) ^^ GetExp |
      "und" ~> par(oneOrMoreVars) ^^ UndExp

  def commands: Parser[List[CommandDef]] =
    repsep(positioned(command), ",")

  def command: Parser[CommandDef] =
    pos(variable) ~ ":=" ~ "{" ~ repsep(pos(variable), ",") ~ "}" ^^ {
      case lhs ~ _ ~ _ ~ rhs ~ _ => CommandDef(lhs, rhs.toSet)
    } |
      pos(variable) ~ ":=" ~ pos(variable) ^^ {
        case lhs ~ _ ~ rhs => CommandDef(lhs, Set(rhs))
      }

  /**
   * A choreography expression
   * - Left associativity: ;,+,||
   * - Precedence: i,(c)>*>;>||>+
   *
   * @return
   */
  def choreography: Parser[ChoreographyExp] =
    pos(maybeParallel) ~ choice ^^ { case mb ~ choice => choice(mb) } |
      pos(maybeParallel)

  def choice: Parser[ChoreographyExp => ChoreographyExp] =
    "+" ~ pos(maybeParallel) ~ choice ^^ {
      case _ ~ mc ~ more => (lhs: ChoreographyExp) => more(ChoiceExp(lhs, mc))
    } |
      "+" ~> pos(maybeParallel) ^^ {
        rhs => (lhs: ChoreographyExp) => ChoiceExp(lhs, rhs)
      }

  def maybeParallel: Parser[ChoreographyExp] =
    pos(maybeSequence) ~ parallel ^^ { case lhs ~ pll => pll(lhs) } |
      pos(maybeSequence)

  def parallel: Parser[ChoreographyExp => ChoreographyExp] =
    "||" ~ pos(maybeSequence) ~ parallel ^^ {
      case _ ~ ms ~ more => (lhs: ChoreographyExp) => more(ParExp(lhs, ms))
    } |
      "||" ~> pos(maybeSequence) ^^ {
        rhs => (lhs: ChoreographyExp) => ParExp(lhs, rhs)
      }

  def maybeSequence: Parser[ChoreographyExp] =
    pos(atomChoreoghrapy) ~ sequence ^^ { case lhs ~ seq => seq(lhs) } |
      pos(atomChoreoghrapy)

  def sequence: Parser[ChoreographyExp => ChoreographyExp] =
    ";" ~ pos(atomChoreoghrapy) ~ sequence ^^ {
      case _ ~ atom ~ more => (lhs: ChoreographyExp) => more(SeqExp(lhs, atom))
    } |
      ";" ~> pos(atomChoreoghrapy) ^^ {
        rhs => (lhs: ChoreographyExp) => SeqExp(lhs, rhs)
      }

  def atomChoreoghrapy: Parser[ChoreographyExp] =
    pos(interaction) | pos(loop) | pos(par(choreography))

  def loop: Parser[LoopExp] =
    par(choreography) <~ "*" ^^ LoopExp

  def interaction: Parser[InteractionExp] =
    variables ~ ">" ~ lowerCaseId ~ opt(par(oneOrMoreVars)) ~ ">" ~ variables ^^ {
      case snd ~ _ ~ ch ~ ms ~ _ ~ rcv => InteractionExp(snd, rcv, ms.getOrElse(List()), ch)
    }
