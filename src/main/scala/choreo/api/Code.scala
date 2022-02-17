package choreo.api

/**
 * Created by guillecledou on 15/02/2022
 */


trait Code:

  override def toString: String = toCode(0)

  def toCode(implicit i:Int):String

  def ind(i:Int):String = " "*i*2

  //def par(arg:List[String],sep:String =","):String = arg match
  //  case h::Nil => h
  //  case _ => arg.mkString("(",sep,")")

  //def bracket(arg:List[String],sep:String=","):String = arg match
  //  case h::Nil => h
  //  case Nil => ""
  //  case _ => arg.mkString("[",",","]")
  //
  //def parInd(arg:Iterable[String],sep:String =",")(implicit i:Int):String = arg.toList match
  //  case h::Nil => h
  //  case _ => arg.map(a=>ind(i)+a).mkString("(\n",sep+"\n",s"\n${ind(Integer.max(i-1,0))})")
  //
  //def bracketInd(arg:Iterable[String],sep:String =",")(implicit i:Int):String = arg.toList match
  //  case h::Nil => h
  //  case _ => arg.map(a=>ind(i)+a).mkString("[\n",sep+"\n",s"\n${ind(Integer.max(i-1,0))}]")

  def brackets(args:List[String],ln:Boolean=true)(implicit i:Int):String =
    if args.isEmpty then ""
    else if !ln || args.size < 4  then args.mkString("[",", ","]")
    else argsLn(args,"[",", ","]")

  def params(args:List[String],sep:String=", ",ln:Boolean=true)(implicit i:Int):String =
    if !ln || args.size < 4 then args.mkString("(",sep,")")
    else argsLn(args,"(",sep,")")

  def argsLn(args:List[String],fst:String,sep:String,lst:String)(implicit i:Int):String =
    args.map(a=>ind(i)+a).mkString(s"$fst\n",sep+"\n",s"\n${ind(i-1)}$lst")

  def length(args:List[String]):Int =
    args.map(_.length).sum