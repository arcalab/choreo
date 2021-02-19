package choreo.choreo2.syntax

import choreo.choreo2.syntax.Choreo._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by guillecledou on 10/02/2021
 */

object Parser extends RegexParsers {

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
  
  def agent: Parser[Agent] = lowerCaseId ^^ Agent
  def agents: Parser[List[Agent]] = repsep(agent, ",")
  
  def message:Parser[Msg] = 
    ":" ~> rep1sep(lowerCaseId,",") ^^ {case ms => Msg(ms)}
  
  /**
   * A choreography expression
   * - Left associativity: ;,+,||
   * - Precedence: i,(c)>*>;>||>+
   *
   * @return
   */
  def choreography: Parser[Choreo] = {
      maybeParallel ~ choice ^^ { case mb ~ choice => choice(mb) } |
      maybeParallel
  }
  
  def choice: Parser[Choreo => Choreo] =
    "+" ~ maybeParallel ~ choice ^^ {
      case _ ~ mc ~ more => (lhs: Choreo) => more(Choice(lhs, mc))
    } |
      "+" ~> maybeParallel ^^ {
        rhs => (lhs: Choreo) => Choice(lhs, rhs)
      }

  def maybeParallel: Parser[Choreo] =
    maybeSequence ~ parallel ^^ { case lhs ~ pll => pll(lhs) } |
      maybeSequence

  def parallel: Parser[Choreo => Choreo] =
    "||" ~ maybeSequence ~ parallel ^^ {
      case _ ~ ms ~ more => (lhs: Choreo) => more(Par(lhs, ms))
    } |
      "||" ~> maybeSequence ^^ {
        rhs => (lhs: Choreo) => Par(lhs, rhs)
      }

  def maybeSequence: Parser[Choreo] =
    maybeOneDChoice ~ sequence ^^ { case lhs ~ seq => seq(lhs) } |
      maybeOneDChoice

  def sequence: Parser[Choreo => Choreo] =
    ";" ~ maybeOneDChoice ~ sequence ^^ {
      case _ ~ odc ~ more => (lhs: Choreo) => more(Seq(lhs, odc))
    } |
      ";" ~> maybeOneDChoice ^^ {
        rhs => (lhs: Choreo) => Seq(lhs, rhs)
      }

  def maybeOneDChoice: Parser[Choreo] =
    atomChoreoghrapy ~ oneDChoice ^^ {case lhs ~ odc => odc(lhs)} |
      atomChoreoghrapy
      
  //todo: change to propoer DChoice construct when starting to add support
  def oneDChoice: Parser [Choreo => Choreo]  =
    "[+]" ~ atomChoreoghrapy ~ oneDChoice ^^ {
      case _ ~ atom ~ more => (lhs:Choreo) => more(Choice(lhs,atom))
    } | 
      "[+]" ~> atomChoreoghrapy ^^ {
        rhs => (lhs:Choreo) => Choice(lhs,rhs)
      }

  def atomChoreoghrapy: Parser[Choreo] =
    endChor | parOrloop | interaction | in | out 

  def endChor: Parser[Choreo] = 
    "0" ^^^ End
    
  def in:Parser[Choreo] =
    agent ~ "?" ~ agent ~ opt(message) ^^ {
      case a ~ _ ~ b ~ ms => In(a,b, ms.getOrElse(Msg(List()))) 
    }

  def out:Parser[Choreo] =
    agent ~ "!" ~ agent ~ opt(message) ^^ {
      case a ~ _ ~ b ~ ms => Out(a, b, ms.getOrElse(Msg(List())))
    }

  def parOrloop: Parser[Choreo] =
    par(choreography) ~ opt("*") ^^ {
      case p ~ l => if l.isDefined then Loop(p) else p
    }

  def interaction: Parser[Send] =
    agents ~ "->" ~ agents ~ opt(message) ^^ {
      case snd ~ _ ~ rcv ~ ms => Send(snd, rcv, ms.getOrElse(Msg(List())))
    }
}