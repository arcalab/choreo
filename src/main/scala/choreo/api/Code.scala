package choreo.api

/**
 * Created by guillecledou on 15/02/2022
 */


trait Code:

  override def toString: String = toCode(0)

  /** Abstract method of Code that can use auxiliary internal functions for prettyprinting */
  def toCode(implicit i:Int):String

  ///////////
  // Auxiliarly functions used for pretty printing when overriding `toCode`
  ///////////

  /** Create indentation spaces for an indentation level `i`. */
  def ind(i:Int):String = " "*i*2

  /** PrettyPrint and indent a list of values wrapped by '[]' and separated by ',' */
  def brackets(args:List[String],ln:Boolean=true)(implicit i:Int):String =
    if args.isEmpty then ""
    else if !ln || args.size < 4  then args.mkString("[",", ","]")
    else argsLn(args,"[",", ","]")

    /** PrettyPrint and indent a list of values wrapped by '()' and separated by ',' */
  def params(args:List[String],sep:String=", ",ln:Boolean=true)(implicit i:Int):String =
    if !ln || (args.size < 4  && args.map(_.length).sum <60) then args.mkString("(",sep,")")
    else argsLn(args,"(",sep,")")

  /** Pretty print and indent a sequence of strings separated by ',' */
  def argsLn(args:List[String],fst:String,sep:String,lst:String)(implicit i:Int):String =
    args.map(a=>ind(i)+a).mkString(s"$fst\n",sep+"\n",s"\n${ind(i-1)}$lst")

  /** Total number of characters */
  def length(args:List[String]):Int =
    args.map(_.length).sum

  private def sep(strings:List[String]):String = strings match
    case Nil => ""
    case _   => strings.mkString("\n\n") ++ "\n\n"